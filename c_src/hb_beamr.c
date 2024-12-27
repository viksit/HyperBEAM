#include "hb_beamr.h"

// Handles import function calls from WASM and interacts with Erlang.
wasm_trap_t *generic_import_handler(void *env, const wasm_val_vec_t *args, wasm_val_vec_t *results)
{
    DRV_DEBUG("generic_import_handler called");

    ImportHook *import_hook = (ImportHook *)env;
    Proc *proc = import_hook->proc;

    
    // Invoke the exported function
    invoke_exported_function(proc, import_hook->field_name, args, results);

    DRV_DEBUG("Proc: %p. Args size: %d", proc, args->size);
    DRV_DEBUG("Import name: %s.%s [%s]", import_hook->module_name, import_hook->field_name, import_hook->signature);

    // Prepare the message for Erlang
    ErlDrvTermData *msg = driver_alloc(sizeof(ErlDrvTermData) * ((2 + (2 * 3)) + ((args->size + 1) * 2) + ((results->size + 1) * 2) + 2));
    int msg_index = 0;

    // Add module and field names to the message
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_import;
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData)import_hook->module_name;
    msg[msg_index++] = strlen(import_hook->module_name);
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData)import_hook->field_name;
    msg[msg_index++] = strlen(import_hook->field_name);

    // Encode arguments
    for (size_t i = 0; i < args->size; i++)
    {
        msg_index += wasm_val_to_erl_term(&msg[msg_index], &args->data[i]);
    }

    // Encode arguments list
    msg[msg_index++] = ERL_DRV_NIL;
    msg[msg_index++] = ERL_DRV_LIST;
    msg[msg_index++] = args->size + 1;

    // Encode function signature
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData)import_hook->signature;
    msg[msg_index++] = strlen(import_hook->signature) - 1;

    // Prepare the message tuple for Erlang
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 5;

    // Initialize import response structure
    proc->current_import = driver_alloc(sizeof(ImportResponse));
    proc->current_import->response_ready = erl_drv_mutex_create("response_mutex");
    proc->current_import->cond = erl_drv_cond_create("response_cond");
    proc->current_import->ready = 0;

    DRV_DEBUG("Sending %d terms...", msg_index);
    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);

    // Wait for the response
    drv_wait(proc->current_import->response_ready, proc->current_import->cond, &proc->current_import->ready);

    DRV_DEBUG("Response ready");

    // Handle errors in the response
    if (proc->current_import->error_message)
    {
        DRV_DEBUG("Import execution failed. Error message: %s", proc->current_import->error_message);
        wasm_name_t message;
        wasm_name_new_from_string_nt(&message, proc->current_import->error_message);
        wasm_trap_t *trap = wasm_trap_new(proc->store, &message);
        driver_free(proc->current_import);
        proc->current_import = NULL;
        return trap;
    }

    // Convert Erlang terms back to WASM values
    const wasm_valtype_vec_t *result_types = wasm_functype_results(wasm_func_type(import_hook->stub_func));
    for (int i = 0; i < proc->current_import->result_length; i++)
    {
        results->data[i].kind = wasm_valtype_kind(result_types->data[i]);
    }

    int res = erl_terms_to_wasm_vals(results, proc->current_import->result_terms);
    if (res == -1)
    {
        DRV_DEBUG("Failed to convert terms to wasm vals");
        return NULL;
    }

    results->num_elems = result_types->num_elems;

    // Clean up the import response
    DRV_DEBUG("Cleaning up import response");
    erl_drv_cond_destroy(proc->current_import->cond);
    erl_drv_mutex_destroy(proc->current_import->response_ready);
    driver_free(proc->current_import);
    proc->current_import = NULL;

    return NULL;
}

// Async initialization function
static void async_init(void* raw) {
    DRV_DEBUG("Initializing WASM module");
    LoadWasmReq* mod_bin = (LoadWasmReq*)raw;
    Proc* proc = mod_bin->proc;
    drv_lock(proc->is_running);
    // Initialize WASM engine, store, etc.

#if HB_DEBUG==1
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_VERBOSE);
#else
    wasm_runtime_set_log_level(WASM_LOG_LEVEL_ERROR);
#endif

    proc->engine = wasm_engine_new();
    DRV_DEBUG("Created engine");
    proc->store = wasm_store_new(proc->engine);
    DRV_DEBUG("Created store");

    // Load WASM module
    wasm_byte_vec_t binary;
    wasm_byte_vec_new(&binary, mod_bin->size, (const wasm_byte_t*)mod_bin->binary);

    proc->module = wasm_module_new(proc->store, &binary);
    DRV_DEBUG("Module created: %p", proc->module);
    if (!proc->module) {
        DRV_DEBUG("Failed to create module");
        wasm_byte_vec_delete(&binary);
        wasm_store_delete(proc->store);
        wasm_engine_delete(proc->engine);
        return;
    }
    //wasm_byte_vec_delete(&binary);
    DRV_DEBUG("Created module");

    // Get imports
    wasm_importtype_vec_t imports;
    wasm_module_imports(proc->module, &imports);
    DRV_DEBUG("Imports size: %d", imports.size);
    wasm_extern_t *stubs[imports.size];

    // Get exports
    wasm_exporttype_vec_t exports;
    wasm_module_exports(proc->module, &exports);

    // Create Erlang lists for imports
    //DRV_DEBUG("Exports size: %d", exports.size);
    ErlDrvTermData* init_msg = driver_alloc(sizeof(ErlDrvTermData) * (2 + (13 * imports.size) + (11 * exports.size)));
    //DRV_DEBUG("Allocated init message");
    int msg_i = 0;
    init_msg[msg_i++] = ERL_DRV_ATOM;
    init_msg[msg_i++] = atom_execution_result;

    // Process imports
    for (int i = 0; i < imports.size; ++i) {
        //DRV_DEBUG("Processing import %d", i);
        const wasm_importtype_t* import = imports.data[i];
        const wasm_name_t* module_name = wasm_importtype_module(import);
        const wasm_name_t* name = wasm_importtype_name(import);
        const wasm_externtype_t* type = wasm_importtype_type(import);

        //DRV_DEBUG("Import: %s.%s", module_name->data, name->data);

        char* type_str = driver_alloc(256);
        // TODO: What happpens here?
        if(!get_function_sig(type, type_str)) {
            // TODO: Handle other types of imports?
            continue;
        }

        init_msg[msg_i++] = ERL_DRV_ATOM;
        init_msg[msg_i++] = driver_mk_atom((char*)wasm_externtype_to_kind_string(type));
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)module_name->data;
        init_msg[msg_i++] = module_name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)name->data;
        init_msg[msg_i++] = name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)type_str;
        init_msg[msg_i++] = strlen(type_str);
        init_msg[msg_i++] = ERL_DRV_TUPLE;
        init_msg[msg_i++] = 4;

        DRV_DEBUG("Creating callback for %s.%s", module_name->data, name->data);
        ImportHook* hook = driver_alloc(sizeof(ImportHook));
        hook->module_name = module_name->data;
        hook->field_name = name->data;
        hook->proc = proc;
        hook->signature = type_str;

        hook->stub_func =
            wasm_func_new_with_env(
                proc->store,
                wasm_externtype_as_functype_const(type),
                generic_import_handler,
                hook,
                NULL
            );
        stubs[i] = wasm_func_as_extern(hook->stub_func);
    }

    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = imports.size + 1;

    // Create proc!
    wasm_extern_vec_t externs;
    wasm_extern_vec_new(&externs, imports.size, stubs);
    wasm_trap_t* trap = NULL;
    proc->instance = wasm_instance_new_with_args(proc->store, proc->module, &externs, &trap, 0x10000, 0x10000);
    if (!proc->instance) {
        DRV_DEBUG("Failed to create WASM proc");
        return;
    }

    // Refresh the exports now that we have an instance
    wasm_module_exports(proc->module, &exports);
    for (size_t i = 0; i < exports.size; i++) {
        //DRV_DEBUG("Processing export %d", i);
        const wasm_exporttype_t* export = exports.data[i];
        const wasm_name_t* name = wasm_exporttype_name(export);
        const wasm_externtype_t* type = wasm_exporttype_type(export);
        char* kind_str = (char*) wasm_externtype_to_kind_string(type);


        char* type_str = driver_alloc(256);
        get_function_sig(type, type_str);
        DRV_DEBUG("Export: %s [%s] -> %s", name->data, kind_str, type_str);

        init_msg[msg_i++] = ERL_DRV_ATOM;
        init_msg[msg_i++] = driver_mk_atom(kind_str);
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)name->data;
        init_msg[msg_i++] = name->size - 1;
        init_msg[msg_i++] = ERL_DRV_STRING;
        init_msg[msg_i++] = (ErlDrvTermData)type_str;
        init_msg[msg_i++] = strlen(type_str);
        init_msg[msg_i++] = ERL_DRV_TUPLE;
        init_msg[msg_i++] = 3;
    }

    init_msg[msg_i++] = ERL_DRV_NIL;
    init_msg[msg_i++] = ERL_DRV_LIST;
    init_msg[msg_i++] = (exports.size) + 1;
    init_msg[msg_i++] = ERL_DRV_TUPLE;
    init_msg[msg_i++] = 3;

    DRV_DEBUG("Sending init message to Erlang. Elements: %d", msg_i);

    int send_res = erl_drv_output_term(proc->port_term, init_msg, msg_i);
    DRV_DEBUG("Send result: %d", send_res);

    proc->current_import = NULL;
    proc->is_initialized = 1;
    drv_unlock(proc->is_running);
}

// Asynchronous call function to execute the function in the WASM module.
static void async_call(void *raw)
{
    Proc *proc = (Proc *)raw;
    DRV_DEBUG("Calling function: %s", proc->current_function);

    drv_lock(proc->is_running);
    char *function_name = proc->current_function;

    // Find the function in the exports
    const wasm_func_t *func = get_exported_function(proc, function_name);
    if (!func)
    {
        send_error(proc, "Function not found: %s", function_name);
        drv_unlock(proc->is_running);
        return;
    }
    DRV_DEBUG("Func: %p", func);

    // Get the function types for parameters and results
    const wasm_functype_t *func_type = wasm_func_type(func);
    const wasm_valtype_vec_t *param_types = wasm_functype_params(func_type);
    const wasm_valtype_vec_t *result_types = wasm_functype_results(func_type);

    wasm_val_vec_t args, results;
    wasm_val_vec_new_uninitialized(&args, param_types->size);
    args.num_elems = param_types->num_elems;

    // Convert Erlang terms to WASM values for parameters
    for (int i = 0; i < param_types->size; i++)
    {
        args.data[i].kind = wasm_valtype_kind(param_types->data[i]);
    }
    int res = erl_terms_to_wasm_vals(&args, proc->current_args);

    if (res == -1)
    {
        send_error(proc, "Failed to convert terms to wasm vals");
        drv_unlock(proc->is_running);
        return;
    }

    // Prepare result vector
    wasm_val_vec_new_uninitialized(&results, result_types->size);
    results.num_elems = result_types->num_elems;
    for (size_t i = 0; i < result_types->size; i++)
    {
        results.data[i].kind = wasm_valtype_kind(result_types->data[i]);
    }

    // // Get the execution environment for the function
    // wasm_exec_env_t exec_env = wasm_runtime_get_exec_env_singleton(func->inst_comm_rt);
    // if (!exec_env) {
    //     DRV_DEBUG("Failed to get execution environment");
    //     return;
    // }

    // Call the function and handle any potential traps (errors)
    DRV_DEBUG("Calling function: %s", function_name);
    wasm_trap_t *trap = wasm_func_call(func, &args, &results);

    if (trap)
    {
        wasm_message_t trap_msg;
        wasm_trap_message(trap, &trap_msg);
        DRV_DEBUG("Trap message: %.*s", trap_msg.size, trap_msg.data);
        // wasm_runtime_dump_call_stack(exec_env);
        // wasm_runtime_dump_mem_consumption(exec_env);

        send_error(proc, "WASM Exception: %.*s", trap_msg.size, trap_msg.data);
        drv_unlock(proc->is_running);
        wasm_trap_delete(trap);
        return;
    }

    // Send the results back to Erlang
    DRV_DEBUG("Results size: %d", results.size);
    ErlDrvTermData *msg = driver_alloc(sizeof(ErlDrvTermData) * (7 + (results.size * 2)));
    int msg_index = 0;
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_execution_result;

    // Encode the results into Erlang terms
    for (size_t i = 0; i < results.size; i++)
    {
        DRV_DEBUG("Processing result %d", i);
        int res_size = wasm_val_to_erl_term(&msg[msg_index], &results.data[i]);
        msg_index += res_size;
    }

    msg[msg_index++] = ERL_DRV_NIL;
    msg[msg_index++] = ERL_DRV_LIST;
    msg[msg_index++] = results.size + 1;
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 2;

    // Send the message back to Erlang
    DRV_DEBUG("Sending %d terms", msg_index);
    int response_msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);
    driver_free(msg);
    DRV_DEBUG("Msg: %d", response_msg_res);

    wasm_val_vec_delete(&results);
    proc->current_import = NULL;

    drv_unlock(proc->is_running);
}

// Initializes the WASM driver for an Erlang port.
static ErlDrvData wasm_driver_start(ErlDrvPort port, char *buff)
{
    ErlDrvSysInfo info;
    driver_system_info(&info, sizeof(info));
    DRV_DEBUG("Starting WASM driver");

    Proc *proc = driver_alloc(sizeof(Proc));
    proc->port = port;
    proc->port_term = driver_mk_port(proc->port);
    proc->is_running = erl_drv_mutex_create("wasm_instance_mutex");
    proc->is_initialized = 0;
    proc->start_time = time(NULL);
    return (ErlDrvData)proc;
}

// Stops the WASM driver and cleans up resources.
static void wasm_driver_stop(ErlDrvData raw)
{
    Proc *proc = (Proc *)raw;
    DRV_DEBUG("Stopping WASM driver");

    // Handle potential pending import response
    if (proc->current_import)
    {
        DRV_DEBUG("Shutting down during import response...");
        proc->current_import->error_message = "WASM driver unloaded during import response";
        proc->current_import->ready = 1;
        drv_signal(proc->current_import->response_ready, proc->current_import->cond, &proc->current_import->ready);
    }

    // Lock, then unlock and destroy mutex to shut down safely
    drv_lock(proc->is_running);
    drv_unlock(proc->is_running);
    erl_drv_mutex_destroy(proc->is_running);

    // Clean up WASM resources
    if (proc->is_initialized)
    {
        DRV_DEBUG("Deleting WASM instance");
        wasm_instance_delete(proc->instance);
        wasm_module_delete(proc->module);
        wasm_store_delete(proc->store);
    }

    // Free allocated resources
    driver_free(proc);
}

// Handles input from the Erlang side to perform specific tasks based on commands.
static void wasm_driver_output(ErlDrvData raw, char *buff, ErlDrvSizeT bufflen)
{
    Proc *proc = (Proc *)raw;
    int index = 0;
    int version;

    if (ei_decode_version(buff, &index, &version) != 0)
    {
        send_error(proc, "Failed to decode message header (version).");
        return;
    }

    int arity;
    ei_decode_tuple_header(buff, &index, &arity);

    char command[MAXATOMLEN];
    ei_decode_atom(buff, &index, command);

    if (strcmp(command, "init") == 0)
    {
        proc->pid = driver_caller(proc->port);
        int size, type;
        ei_get_type(buff, &index, &type, &size);
        void *wasm_binary = driver_alloc(size);
        long size_l = (long)size;
        ei_decode_binary(buff, &index, wasm_binary, &size_l);
        LoadWasmReq *mod_bin = driver_alloc(sizeof(LoadWasmReq));
        mod_bin->proc = proc;
        mod_bin->binary = wasm_binary;
        mod_bin->size = size;

        driver_async(proc->port, NULL, async_init, mod_bin, NULL);
    }
    else if (strcmp(command, "call") == 0)
    {
        if (!proc->is_initialized)
        {
            send_error(proc, "Cannot run WASM function as module not initialized.");
            return;
        }

        // Handle function call
        char *function_name = driver_alloc(MAXATOMLEN);
        ei_decode_string(buff, &index, function_name);
        proc->current_function = function_name;

        proc->current_args = decode_list(buff, &index);
        driver_async(proc->port, NULL, async_call, proc, NULL);
    }
    else if (strcmp(command, "import_response") == 0)
    {
        if (proc->current_import)
        {
            proc->current_import->result_terms = decode_list(buff, &index);
            proc->current_import->error_message = NULL;
            drv_signal(proc->current_import->response_ready, proc->current_import->cond, &proc->current_import->ready);
        }
        else
        {
            send_error(proc, "No pending import response waiting");
        }
    }
    else if (strcmp(command, "write") == 0)
    {
        // Handle memory write operation
        long ptr, size;
        int type;
        ei_decode_tuple_header(buff, &index, &arity);
        ei_decode_long(buff, &index, &ptr);
        ei_get_type(buff, &index, &type, &size);
        long size_l = (long)size;
        char *wasm_binary;
        ei_decode_bitstring(buff, &index, &wasm_binary, NULL, &size_l);
        long size_bytes = size_l / 8;
        byte_t *memory_data = wasm_memory_data(get_memory(proc));
        memcpy(memory_data + ptr, wasm_binary, size_bytes);

        ErlDrvTermData *msg = driver_alloc(sizeof(ErlDrvTermData) * 2);
        msg[0] = ERL_DRV_ATOM;
        msg[1] = atom_ok;
        erl_drv_output_term(proc->port_term, msg, 2);
    }
    else if (strcmp(command, "read") == 0)
    {
        // Handle memory read operation
        long ptr, size;
        ei_decode_tuple_header(buff, &index, &arity);
        ei_decode_long(buff, &index, &ptr);
        ei_decode_long(buff, &index, &size);
        long size_l = (long)size;
        byte_t *memory_data = wasm_memory_data(get_memory(proc));
        char *out_binary = driver_alloc(size_l);
        memcpy(out_binary, memory_data + ptr, size_l);

        ErlDrvTermData *msg = driver_alloc(sizeof(ErlDrvTermData) * 7);
        int msg_index = 0;
        msg[msg_index++] = ERL_DRV_ATOM;
        msg[msg_index++] = atom_execution_result;
        msg[msg_index++] = ERL_DRV_BUF2BINARY;
        msg[msg_index++] = (ErlDrvTermData)out_binary;
        msg[msg_index++] = size_l;
        msg[msg_index++] = ERL_DRV_TUPLE;
        msg[msg_index++] = 2;

        erl_drv_output_term(proc->port_term, msg, msg_index);
    }
    else if (strcmp(command, "size") == 0)
    {
        // Handle size query for memory
        wasm_memory_t *mem = get_memory(proc);
        long pages = wasm_memory_size(mem);
        long size = pages * 65536;

        ErlDrvTermData *msg = driver_alloc(sizeof(ErlDrvTermData) * 6);
        msg[0] = ERL_DRV_ATOM;
        msg[1] = atom_execution_result;
        msg[2] = ERL_DRV_INT;
        msg[3] = size;
        msg[4] = ERL_DRV_TUPLE;
        msg[5] = 2;
        erl_drv_output_term(proc->port_term, msg, 6);
    }
    else
    {
        send_error(proc, "Unknown command");
    }
}

// WASM driver entry structure
static ErlDrvEntry wasm_driver_entry = {
    NULL,
    wasm_driver_start,
    wasm_driver_stop,
    wasm_driver_output,
    NULL,
    NULL,
    "hb_beamr",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,
    NULL,
    NULL};

// Initialize the driver and define atoms
DRIVER_INIT(wasm_driver)
{
    atom_ok = driver_mk_atom("ok");
    atom_error = driver_mk_atom("error");
    atom_import = driver_mk_atom("import");
    atom_execution_result = driver_mk_atom("execution_result");
    return &wasm_driver_entry;
}
