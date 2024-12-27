#include "hb_beamr.h"

#include <stdio.h>
#include <stdarg.h>
#include <pthread.h>
#include <string.h>
#include <stdlib.h>

// Function to print debug messages with thread info and location
void beamr_print(int print, const char* file, int line, const char* format, ...) {
    va_list args;

    // Initialize variable argument list for the function
    va_start(args, format);

    // Check if the 'print' flag is set to 1 to print the debug message
    if(print) {
        // Get the current thread ID to print along with the debug message
        pthread_t thread_id = pthread_self();

        // Print thread ID and the source location (file and line number)
        printf("[DBG#%p @ %s:%d] ", thread_id, file, line);

        // Print the formatted debug message using vprintf with the variable arguments
        vprintf(format, args);

        // Print a new line at the end for clarity
        printf("\r\n");
    }

    // Clean up the variable argument list
    va_end(args);
}

// Function to send an error message to the Erlang process
void send_error(Proc* proc, const char* message_fmt, ...) {
    va_list args;

    // Initialize variable argument list for the error message
    va_start(args, message_fmt);

    // Allocate memory for the formatted error message (256 characters)
    char* message = driver_alloc(256);

    // Format the error message using vsnprintf (safe formatting to avoid overflow)
    vsnprintf(message, 256, message_fmt, args);

    // Debug: Log the message before sending
    DRV_DEBUG("Sending error message: %s", message);

    // Allocate memory for Erlang term data (to construct the error message in Erlang format)
    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * 7);
    int msg_index = 0;

    // Construct the Erlang term message with the following structure:
    // ERL_DRV_ATOM: Atom for the error (atom_error)
    msg[msg_index++] = ERL_DRV_ATOM;
    msg[msg_index++] = atom_error;

    // ERL_DRV_STRING: The actual error message string
    msg[msg_index++] = ERL_DRV_STRING;
    msg[msg_index++] = (ErlDrvTermData)message;  // Pointer to the message string
    msg[msg_index++] = strlen(message);  // Length of the message

    // ERL_DRV_TUPLE: Wrap the message in a tuple of size 2
    msg[msg_index++] = ERL_DRV_TUPLE;
    msg[msg_index++] = 2;

    // Send the error message to the Erlang process using the driver's output term function
    int msg_res = erl_drv_output_term(proc->port_term, msg, msg_index);

    // Debug: Log the result of sending the error message
    DRV_DEBUG("Sent error message. Res: %d", msg_res);

    // Clean up the variable argument list
    va_end(args);
}
