#include "hb_beamr.h"

// Checks if the given export name corresponds to the indirect function table
static bool is_indirect_function_table(const char *export_name) {
    return strcmp(export_name, "__indirect_function_table") == 0;
}

// Prepares the function arguments by removing the first argument (function index)
static void prepare_function_arguments(const wasm_val_vec_t *input_args, wasm_val_vec_t *prepared_args) {
    // If there are no arguments or only one argument (function index), no preparation is needed
    if (input_args->size <= 1) {
        DRV_DEBUG("Not enough arguments to create new wasm_val_vec_t");
        return;
    }

    // Allocate memory for the prepared arguments
    wasm_val_t* prepared_data = malloc(sizeof(wasm_val_t) * (input_args->size - 1));

    // Copy the arguments starting from the second element (skip function index)
    for (size_t i = 1; i < input_args->size; ++i) {
        prepared_data[i - 1] = input_args->data[i];
    }

    // Create a new wasm_val_vec_t with the prepared arguments
    wasm_val_vec_new(prepared_args, input_args->size - 1, prepared_data);
    DRV_DEBUG("Prepared %zu arguments for function call", prepared_args->size);
}

// Calls the specified WASM function using an indirect call
static void handle_wasm_function_call(wasm_exec_env_t exec_env, int function_index, uint64_t argc, uint64_t* argv) {
    DRV_DEBUG("Calling WASM function with indirect call...");
    
    // Attempt to call the function and check for any exceptions
    if (!wasm_runtime_call_indirect(exec_env, function_index, argc, argv)) {
        if (wasm_runtime_get_exception(exec_env)) {
            DRV_DEBUG("%s", wasm_runtime_get_exception(exec_env));
        }
        DRV_DEBUG("WASM function call failed");
    }
}

// Main function to invoke an exported function from the WASM instance
void invoke_exported_function(Proc* proc, const char *field_name, const wasm_val_vec_t* input_args, wasm_val_vec_t* output_results) {
    DRV_DEBUG("=================================================");
    DRV_DEBUG("Starting function invocation for field: %s", field_name);
    DRV_DEBUG("=================================================");

    // Check if the field name is "invoke"; if not, exit early
    if (strncmp(field_name, "invoke", 6) != 0) {
        DRV_DEBUG("Field name '%s' is not an invoke operation", field_name);
        return;
    }

    // Retrieve exported functions from the WASM instance and module
    wasm_extern_vec_t exported_externs;
    wasm_instance_exports(proc->instance, &exported_externs);

    wasm_exporttype_vec_t export_types;
    wasm_module_exports(proc->module, &export_types);

    DRV_DEBUG("Retrieved %zu exported externs from the instance.", exported_externs.size);
    DRV_DEBUG("Retrieved %zu export types from the module.", export_types.size);

    // Iterate over the exported types and search for the indirect function table
    for (int i = 0; i < export_types.size; i++) {
        const wasm_exporttype_t* export = export_types.data[i];
        const wasm_name_t* export_name = wasm_exporttype_name(export);

        // Check if the export is the indirect function table
        if (is_indirect_function_table(export_name->data)) {
            DRV_DEBUG("-------------------------------------------------");
            DRV_DEBUG("Found indirect function table: %.*s", export_name->size, export_name->data);
            DRV_DEBUG("-------------------------------------------------");

            // Retrieve the function table and its limits
            const wasm_externtype_t* export_type = wasm_exporttype_type(export);
            const wasm_tabletype_t* table_type = wasm_externtype_as_tabletype_const(export_type);
            const wasm_limits_t* table_limits = wasm_tabletype_limits(table_type);
            DRV_DEBUG("Table limits: max=%d", table_limits->max);

            // Retrieve the indirect function table
            const wasm_table_t* indirect_function_table = wasm_extern_as_table(exported_externs.data[i]);

            // Extract the function index from the input arguments
            int function_index = input_args->data[0].of.i32;  
            DRV_DEBUG("Function index retrieved from input_args: %d", function_index);

            // Get the function reference from the table and cast it to a function
            wasm_ref_t* function_ref = wasm_table_get(indirect_function_table, function_index);
            const wasm_func_t* func = wasm_ref_as_func(function_ref);
            DRV_DEBUG("Function pointer: %p", func);

            // Retrieve the function type and log its parameters and results
            const wasm_functype_t* function_type = wasm_func_type(func);
            if (!function_type) {
                DRV_DEBUG("Failed to retrieve function type for function at index %d", function_index);
                continue;
            }

            // Log the function's parameter types
            const wasm_valtype_vec_t* param_types = wasm_functype_params(function_type);
            DRV_DEBUG("Function at index %d has %zu parameters", function_index, param_types->size);
            for (size_t j = 0; j < param_types->size; ++j) {
                const wasm_valtype_t* param_type = param_types->data[j];
                wasm_valkind_t param_kind = wasm_valtype_kind(param_type);
                DRV_DEBUG("Param %zu: %s", j, get_wasm_type_name(param_kind));
            }

            // Log the function's result types
            const wasm_valtype_vec_t* result_types = wasm_functype_results(function_type);
            DRV_DEBUG("Function at index %d has %zu results", function_index, result_types->size);
            for (size_t k = 0; k < result_types->size; ++k) {
                const wasm_valtype_t* result_type = result_types->data[k];
                wasm_valkind_t result_kind = wasm_valtype_kind(result_type);
                DRV_DEBUG("Result %zu: %s", k, get_wasm_type_name(result_kind));
            }

            // Prepare the arguments for the function call
            wasm_val_vec_t prepared_args;
            prepare_function_arguments(input_args, &prepared_args);

            // Get the execution environment for the function
            wasm_exec_env_t exec_env = wasm_runtime_get_exec_env_singleton(func->inst_comm_rt);
            if (!exec_env) {
                DRV_DEBUG("Failed to get execution environment");
                return;
            }
            
            uint64_t argc = prepared_args.size;
            uint64_t* argv = malloc(sizeof(uint64_t) * argc);
            
            // Convert prepared arguments to an array of 64-bit integers
            for (uint64_t i = 0; i < argc; ++i) {
                argv[i] = prepared_args.data[i].of.i64;
            }


            // Call the function with the prepared arguments
            handle_wasm_function_call(exec_env, function_index, argc, argv);

            // Free allocated memory
            free(argv);
            free(prepared_args.data);
            DRV_DEBUG("Function call completed successfully");
        }
    }

    DRV_DEBUG("=================================================");
    DRV_DEBUG("Invoke function completed successfully for field: %s", field_name);
    DRV_DEBUG("=================================================\n");
}
