#include "hb_beamr.h"

wasm_func_t* get_exported_function(Proc* proc, const char* target_name) {
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(proc->module, &export_types);
    wasm_func_t* func = NULL;

    for (size_t i = 0; i < exports.size; i++) {
        const wasm_exporttype_t* export = exports.data[i];
        const wasm_name_t* name = wasm_exporttype_name(export);
        const wasm_externtype_t* type = wasm_exporttype_type(export);
        DRV_DEBUG("Export: %.*s [%s]", name->size, name->data, wasm_externtype_to_kind_string(type));
    }

    for (size_t i = 0; i < exports.size; ++i) {
        wasm_extern_t* ext = exports.data[i];
        if (wasm_extern_kind(ext) == WASM_EXTERN_FUNC) {
            const wasm_name_t* exp_name = wasm_exporttype_name(export_types.data[i]);
            if (exp_name && exp_name->size == strlen(target_name) + 1 && 
                strncmp(exp_name->data, target_name, exp_name->size - 1) == 0) {
                func = wasm_extern_as_func(ext);
                break;
            }
        }
    }

    return func;
}

wasm_memory_t* get_memory(Proc* proc) {
    wasm_extern_vec_t exports;
    wasm_instance_exports(proc->instance, &exports);
    for (size_t i = 0; i < exports.size; i++) {
        if (wasm_extern_kind(exports.data[i]) == WASM_EXTERN_MEMORY) {
            return wasm_extern_as_memory(exports.data[i]);
        }
    }
    return NULL;
}

