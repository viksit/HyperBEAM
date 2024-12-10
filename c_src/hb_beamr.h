#ifndef HB_BEAMR_H
#define HB_BEAMR_H
#include <erl_driver.h>
#include <ei.h>
#include <wasm_c_api.h>
#include <wasm_export.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <pthread.h>

#define HB_DEBUG 1

typedef struct {
    ErlDrvMutex* response_ready;
    ErlDrvCond* cond;
    int ready;
    char* error_message;
    ei_term* result_terms;
    int result_length;
} ImportResponse;

typedef struct {
    wasm_engine_t* engine;
    wasm_instance_t* instance;
    wasm_module_t* module;
    wasm_store_t* store;
    ErlDrvPort port;
    ErlDrvTermData port_term;
    ErlDrvMutex* is_running;
    char* current_function;
    ei_term* current_args;
    int current_args_length;
    ImportResponse* current_import;
    ErlDrvTermData pid;
    int is_initialized;
    time_t start_time;
} Proc;

typedef struct {
    char* module_name;
    char* field_name;
    char* signature;
    Proc* proc;
    wasm_func_t* stub_func;
} ImportHook;

typedef struct {
    void* binary;
    long size;
    Proc* proc;
} LoadWasmReq;

static ErlDrvTermData atom_ok;
static ErlDrvTermData atom_error;
static ErlDrvTermData atom_import;
static ErlDrvTermData atom_execution_result;


/* Beamr */
void invoke_exported_function(Proc* proc, const char * field_name, const wasm_val_vec_t* args, wasm_val_vec_t* results);
wasm_trap_t* generic_import_handler(void* env, const wasm_val_vec_t* args, wasm_val_vec_t* results);
static void async_init(void* raw);
static void async_call(void* raw);
static ErlDrvData wasm_driver_start(ErlDrvPort port, char *buff);
static void wasm_driver_stop(ErlDrvData raw);
static void wasm_driver_output(ErlDrvData raw, char *buff, ErlDrvSizeT bufflen);

/* Utils */
wasm_val_t create_default_wasm_val();
const char* get_wasm_type_name(wasm_valkind_t kind);
const char* wasm_externtype_to_kind_string(const wasm_externtype_t* type);
char wasm_valtype_kind_to_char(const wasm_valtype_t* valtype);
int wasm_val_to_erl_term(ErlDrvTermData* term, const wasm_val_t* val);
int erl_term_to_wasm_val(wasm_val_t* val, ei_term* term);
int erl_terms_to_wasm_vals(wasm_val_vec_t* vals, ei_term* terms);

ei_term* decode_list(char* buff, int* index);
int get_function_sig(const wasm_externtype_t* type, char* type_str);

void drv_lock(ErlDrvMutex* mutex);
void drv_unlock(ErlDrvMutex* mutex);
void drv_signal(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready);
void drv_wait(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready);

/* Logging */
void beamr_print(int print, const char* file, int line, const char* format, ...);
void send_error(Proc* proc, const char* message_fmt, ...);

/* Exports / Memory */
wasm_func_t* get_exported_function(Proc* proc, const char* target_name);
wasm_memory_t* get_memory(Proc* proc);


#define DRV_DEBUG(format, ...) beamr_print(HB_DEBUG, __FILE__, __LINE__, format, ##__VA_ARGS__)
#define DRV_PRINT(format, ...) beamr_print(1, __FILE__, __LINE__, format, ##__VA_ARGS__)

#endif /* HB_BEAMR_H */
