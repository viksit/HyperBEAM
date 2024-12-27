#ifndef HB_BEAMR_H
#define HB_BEAMR_H

// Standard includes
#include <erl_driver.h>
#include <ei.h>
#include <wasm_c_api.h>
#include <wasm_export.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <pthread.h>

// Debugging flag: Enables debug logs when set to 1
#define HB_DEBUG 1

// Structure to represent the response for an import operation
typedef struct {
    ErlDrvMutex* response_ready;    // Mutex to synchronize response readiness
    ErlDrvCond* cond;               // Condition variable to signal readiness
    int ready;                       // Flag indicating if the response is ready
    char* error_message;            // Error message (if any)
    ei_term* result_terms;          // List of result terms from the import
    int result_length;              // Length of the result_terms
} ImportResponse;

// Structure to represent a WASM process instance
typedef struct {
    wasm_engine_t* engine;          // WASM engine instance
    wasm_instance_t* instance;      // WASM instance
    wasm_module_t* module;          // WASM module
    wasm_store_t* store;            // WASM store
    ErlDrvPort port;                // Erlang port associated with this process
    ErlDrvTermData port_term;       // Erlang term representation of the port
    ErlDrvMutex* is_running;        // Mutex to track if the process is running
    char* current_function;        // Current function being executed
    ei_term* current_args;         // Arguments for the current function
    int current_args_length;       // Length of the current arguments
    ImportResponse* current_import; // Import response structure
    ErlDrvTermData pid;            // PID of the Erlang process
    int is_initialized;            // Flag to check if the process is initialized
    time_t start_time;             // Start time of the process
} Proc;

// Structure to represent an import hook
typedef struct {
    char* module_name;             // Name of the module
    char* field_name;              // Name of the field (function)
    char* signature;               // Function signature
    Proc* proc;                    // The associated process
    wasm_func_t* stub_func;        // WASM function pointer for the import
} ImportHook;

// Structure to represent the request for loading a WASM binary
typedef struct {
    void* binary;                  // Binary data for the WASM module
    long size;                     // Size of the binary
    Proc* proc;                    // The associated process
} LoadWasmReq;

// Structure for a common WASM module instance
typedef struct WASMModuleInstanceCommon {
    uint32_t module_type;          // Type of the module
    uint8_t module_inst_data[1];   // Module instance data
} WASMModuleInstanceCommon;

// Structure to store host information about the WASM instance
struct wasm_host_info {
    void *info;                        // Pointer to host info
    void (*finalizer)(void *);         // Finalizer function for the host info
};

// Structure representing a WASM function (extended with host-specific details)
struct wasm_func_t {
    wasm_store_t *store;             // WASM store
    wasm_name_t *module_name;        // Module name for the function
    wasm_name_t *name;               // Function name
    uint16_t kind;                   // Function kind (e.g., export)
    struct wasm_host_info host_info; // Host-specific information
    wasm_functype_t *type;          // Function type (parameters and results)
    uint16_t param_count;            // Number of parameters
    uint16_t result_count;           // Number of results
    bool with_env;                   // Whether the function has an environment
    union {
        wasm_func_callback_t cb;         // Callback function
        struct callback_ext {
            void *env;                  // Environment for the callback
            wasm_func_callback_with_env_t cb; // Callback function with environment
            void (*finalizer)(void *);  // Finalizer for the callback
        } cb_env;
    } u;
    uint16_t func_idx_rt;            // Function index in the runtime
    WASMModuleInstanceCommon *inst_comm_rt; // Module instance data
    WASMFunctionInstanceCommon *func_comm_rt; // Function instance data
};

// Declare the atoms used in Erlang driver communication
static ErlDrvTermData atom_ok;
static ErlDrvTermData atom_error;
static ErlDrvTermData atom_import;
static ErlDrvTermData atom_execution_result;

/* Function Prototypes */

// Generic import handler for WASM functions
wasm_trap_t* generic_import_handler(void* env, const wasm_val_vec_t* args, wasm_val_vec_t* results);

// Asynchronous initialization and function call handlers
static void async_init(void* raw);
static void async_call(void* raw);

// Driver-related functions for starting, stopping, and handling input/output
static ErlDrvData wasm_driver_start(ErlDrvPort port, char *buff);
static void wasm_driver_stop(ErlDrvData raw);
static void wasm_driver_output(ErlDrvData raw, char *buff, ErlDrvSizeT bufflen);

// Function to invoke a WASM exported function
void invoke_exported_function(Proc* proc, const char * field_name, const wasm_val_vec_t* args, wasm_val_vec_t* results);

// Functions for exporting and memory access
wasm_func_t* get_exported_function(Proc* proc, const char* target_name);
wasm_memory_t* get_memory(Proc* proc);

// Utility functions for type conversion and term handling
const char* get_wasm_type_name(wasm_valkind_t kind);
const char* wasm_externtype_to_kind_string(const wasm_externtype_t* type);
char wasm_valtype_kind_to_char(const wasm_valtype_t* valtype);
int wasm_val_to_erl_term(ErlDrvTermData* term, const wasm_val_t* val);
int erl_term_to_wasm_val(wasm_val_t* val, ei_term* term);
int erl_terms_to_wasm_vals(wasm_val_vec_t* vals, ei_term* terms);

// Function to decode an Erlang list from a binary buffer
ei_term* decode_list(char* buff, int* index);

// Function to get the function signature from a wasm_externtype_t
int get_function_sig(const wasm_externtype_t* type, char* type_str);

// Functions for handling Erlang mutexes and condition variables
void drv_lock(ErlDrvMutex* mutex);
void drv_unlock(ErlDrvMutex* mutex);
void drv_signal(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready);
void drv_wait(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready);

// Logging functions
void beamr_print(int print, const char* file, int line, const char* format, ...);
void send_error(Proc* proc, const char* message_fmt, ...);

// Macros for logging with file and line number
#define DRV_DEBUG(format, ...) beamr_print(HB_DEBUG, __FILE__, __LINE__, format, ##__VA_ARGS__)
#define DRV_PRINT(format, ...) beamr_print(1, __FILE__, __LINE__, format, ##__VA_ARGS__)

#endif /* HB_BEAMR_H */
