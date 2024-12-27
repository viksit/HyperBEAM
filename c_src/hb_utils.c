#include "hb_beamr.h"


// Returns the string name corresponding to the wasm type
const char* get_wasm_type_name(wasm_valkind_t kind) {
    switch (kind) {
        case WASM_I32: return "i32";
        case WASM_I64: return "i64";
        case WASM_F32: return "f32";
        case WASM_F64: return "f64";
        default: return "unknown";
    }
}

// Returns the string name corresponding to the external type (e.g., func, global, table, memory)
const char* wasm_externtype_to_kind_string(const wasm_externtype_t* type) {
    switch (wasm_externtype_kind(type)) {
        case WASM_EXTERN_FUNC: return "func";
        case WASM_EXTERN_GLOBAL: return "global";
        case WASM_EXTERN_TABLE: return "table";
        case WASM_EXTERN_MEMORY: return "memory";
        default: return "unknown";
    }
}

// Helper function to convert wasm_valtype_t to char (used for function signatures)
char wasm_valtype_kind_to_char(const wasm_valtype_t* valtype) {
    switch (wasm_valtype_kind(valtype)) {
        case WASM_I32: return 'i';
        case WASM_I64: return 'I';
        case WASM_F32: return 'f';
        case WASM_F64: return 'F';
        case WASM_EXTERNREF: return 'e';
        case WASM_V128: return 'v';
        case WASM_FUNCREF: return 'f';
        default: return 'u';
    }
}

// Converts a wasm_val to an Erlang term (based on its type)
int wasm_val_to_erl_term(ErlDrvTermData* term, const wasm_val_t* val) {
    DRV_DEBUG("Adding wasm val to erl term");
    DRV_DEBUG("Val of: %d", val->of.i32);
    switch (val->kind) {
        case WASM_I32:
            term[0] = ERL_DRV_INT;
            term[1] = val->of.i32;
            return 2;
        case WASM_I64:
            term[0] = ERL_DRV_INT64;
            term[1] = (ErlDrvTermData) &val->of.i64;
            return 2;
        case WASM_F32:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData) &val->of.f32;
            return 2;
        case WASM_F64:
            term[0] = ERL_DRV_FLOAT;
            term[1] = (ErlDrvTermData) &val->of.f64;
            return 2;
        default:
            DRV_DEBUG("Unsupported result type: %d", val->kind);
            return 0;
    }
}

// Converts an Erlang term to a wasm_val (converts based on type)
int erl_term_to_wasm_val(wasm_val_t* val, ei_term* term) {
    DRV_DEBUG("Converting erl term to wasm val. Term: %d. Size: %d", term->value.i_val, term->size);
    switch (val->kind) {
        case WASM_I32:
            val->of.i32 = (int) term->value.i_val;
            break;
        case WASM_I64:
            val->of.i64 = (long) term->value.i_val;
            break;
        case WASM_F32:
            val->of.f32 = (float) term->value.d_val;
            break;
        case WASM_F64:
            val->of.f64 = term->value.d_val;
            break;
        default:
            DRV_DEBUG("Unsupported parameter type: %d", val->kind);
            return -1;
    }
    return 0;
}

// Converts an array of Erlang terms to an array of wasm_vals
int erl_terms_to_wasm_vals(wasm_val_vec_t* vals, ei_term* terms) {
    DRV_DEBUG("Converting erl terms to wasm vals");
    DRV_DEBUG("Vals: %d", vals->size);
    for (int i = 0; i < vals->size; i++) {
        DRV_DEBUG("Converting term %d: %p", i, &vals->data[i]);
        int res = erl_term_to_wasm_val(&vals->data[i], &terms[i]);
        if (res == -1) {
            DRV_DEBUG("Failed to convert term to wasm val");
            return -1;
        }
    }
    return 0;
}

// Decodes an Erlang list from a binary buffer
ei_term* decode_list(char* buff, int* index) {
    int arity, type;

    if (ei_get_type(buff, index, &type, &arity) == -1) {
        DRV_DEBUG("Failed to get type");
        return NULL;
    }
    DRV_DEBUG("Decoded header. Arity: %d", arity);

    ei_term* res = driver_alloc(sizeof(ei_term) * arity);

    // If the type is a list, decode it
    if (type == ERL_LIST_EXT) {
        ei_decode_list_header(buff, index, &arity);
        for (int i = 0; i < arity; i++) {
            ei_decode_ei_term(buff, index, &res[i]);
            DRV_DEBUG("Decoded term (assuming int) %d: %d", i, res[i].value.i_val);
        }
    }
    // If the type is a string, decode it as a list of integers
    else if (type == ERL_STRING_EXT) {
        unsigned char* str = driver_alloc(arity * sizeof(char) + 1);
        ei_decode_string(buff, index, str);
        for (int i = 0; i < arity; i++) {
            res[i].ei_type = ERL_INTEGER_EXT;
            res[i].value.i_val = (long) str[i];
            DRV_DEBUG("Decoded term %d: %d", i, res[i].value.i_val);
        }
        driver_free(str);
    } 
    // Handle unknown types
    else {
        DRV_DEBUG("Unknown type: %d", type);
        return NULL;
    }

    return res;
}

// Retrieves the function signature from the external type and formats it as a string
int get_function_sig(const wasm_externtype_t* type, char* type_str) {
    if (wasm_externtype_kind(type) == WASM_EXTERN_FUNC) {
        const wasm_functype_t* functype = wasm_externtype_as_functype_const(type);
        const wasm_valtype_vec_t* params = wasm_functype_params(functype);
        const wasm_valtype_vec_t* results = wasm_functype_results(functype);

        if (!params || !results) {
            DRV_DEBUG("Export function params/results are NULL");
            return 0;
        }

        // Format the function signature
        type_str[0] = '(';
        size_t offset = 1;

        for (size_t i = 0; i < params->size; i++) {
            type_str[offset++] = wasm_valtype_kind_to_char(params->data[i]);
        }
        type_str[offset++] = ')';

        for (size_t i = 0; i < results->size; i++) {
            type_str[offset++] = wasm_valtype_kind_to_char(results->data[i]);
        }
        type_str[offset] = '\0';

        return 1;
    }
    return 0;
}

// Locks the given ErlDrvMutex
void drv_lock(ErlDrvMutex* mutex) {
    DRV_DEBUG("Locking: %s", erl_drv_mutex_name(mutex));
    erl_drv_mutex_lock(mutex);
    DRV_DEBUG("Locked: %s", erl_drv_mutex_name(mutex));
}

// Unlocks the given ErlDrvMutex
void drv_unlock(ErlDrvMutex* mutex) {
    DRV_DEBUG("Unlocking: %s", erl_drv_mutex_name(mutex));
    erl_drv_mutex_unlock(mutex);
    DRV_DEBUG("Unlocked: %s", erl_drv_mutex_name(mutex));
}

// Signals the condition variable, setting ready to 1
void drv_signal(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready) {
    DRV_DEBUG("Signaling: %s. Pre-signal ready state: %d", erl_drv_cond_name(cond), *ready);
    drv_lock(mut);
    *ready = 1;
    erl_drv_cond_signal(cond);
    drv_unlock(mut);
    DRV_DEBUG("Signaled: %s. Post-signal ready state: %d", erl_drv_cond_name(cond), *ready);
}

// Waits for the condition variable to be signaled, locking the mutex during the wait
void drv_wait(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready) {
    DRV_DEBUG("Started to wait: %s. Ready: %d", erl_drv_cond_name(cond), *ready);
    DRV_DEBUG("Mutex: %s", erl_drv_mutex_name(mut));
    drv_lock(mut);
    while (!*ready) {
        DRV_DEBUG("Waiting: %s", erl_drv_cond_name(cond));
        erl_drv_cond_wait(cond, mut);
        DRV_DEBUG("Woke up: Ready: %d", *ready);
    }
    drv_unlock(mut);
    DRV_DEBUG("Finish waiting: %s", erl_drv_cond_name(cond));
}