#include "hb_beamr.h"

// Retrieves the exported function from the WASM instance by its target name.
wasm_func_t* get_exported_function(Proc* proc, const char* target_name) {
    // Get the list of exported externs from the instance
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);

    // Get the list of export types from the module
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(proc->module, &export_types);

    wasm_func_t* func = NULL;

    // Iterate through exports to find the function matching the target_name
    for (size_t i = 0; i < exports.size; ++i) {
        wasm_extern_t* ext = exports.data[i];
        
        // Check if the export is a function
        if (wasm_extern_kind(ext) == WASM_EXTERN_FUNC) {
            const wasm_name_t* exp_name = wasm_exporttype_name(export_types.data[i]);
            
            // If the name matches the target_name, return the function
            if (exp_name && exp_name->size == strlen(target_name) + 1 && 
                strncmp(exp_name->data, target_name, exp_name->size - 1) == 0) {
                func = wasm_extern_as_func(ext);
                break;
            }
        }
    }

    return func;
}

// Retrieves the memory associated with the WASM instance.
wasm_memory_t* get_memory(Proc* proc) {
    // Get the list of exported externs from the instance
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);

    // Iterate through the exports and find the memory export
    for (size_t i = 0; i < exports.size; i++) {
        if (wasm_extern_kind(exports.data[i]) == WASM_EXTERN_MEMORY) {
            return wasm_extern_as_memory(exports.data[i]);
        }
    }

    // Return NULL if no memory export is found
    return NULL;
}