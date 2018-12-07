/* Copyright 2004--2009 Bluespec, Inc.  All rights reserved. */

#ifndef __BLUESIM_KERNEL_H__
#define __BLUESIM_KERNEL_H__

#include "bluesim_types.h"

/*
 * Declarations of all functions in the Bluesim kernel API.
 * All functions have C linkage.
 */

#if __cplusplus
extern "C" {
#endif

/*
 * Kernel resource management routines.
 */

/* This must be called before calling any other Bluesim
 * kernel API functions.
 * When master is non-zero, it indicates that the model
 * is its own master.
 * When wait_for_license is non-zero, it indicates that the
 * kernel should wait for a license if it is not available.
 *
 * Returns a handle to the simulation state, which is needed
 * as an argument to the other Bluesim kernel API functions.
 */
tSimStateHdl bk_init(tModel model, tBool master, tBool wait_for_license);

/* This should be called at the end of simulation
 * to free resources controlled by the simulation kernel.
 * After bk_shutdown() is called, no other Bluesim kernel
 * API functions may be called unless bk_init() has been
 * called first.
 */
void bk_shutdown(tSimStateHdl simHdl);

/* Get version information about the Bluesim model */
void bk_version(tSimStateHdl simHdl, tBluesimVersionInfo* version);

/*
 * Kernel clock definition
 */

/* Define a 2-phase clock waveform to be generated by the
 * Bluesim kernel.
 *
 *   name              - the name associated with the clock domain
 *   initial_value     - the value of the clock before the first edge
 *   has_initial_value - whether the clock gets the initial value or X
 *   first_edge        - the delay until the first edge
 *   low_duration      - the duration the clock remains CLK_LOW
 *   high_duration     - the duration the clock remains CLK_HIGH
 *
 * Returns the handle for the newly generated clock.
 *
 * Note: the total clock period is (low_duration + high_duration),
 * and a 50% duty-cycle is obtained when low_duration = high_duration.
 *
 * Note: when the total period is 0, it indicates that the clock is
 * to be managed explicitly by calling bk_trigger_clock_edge().
 */
tClock bk_define_clock(tSimStateHdl simHdl,
		       const char* name,
		       tClockValue initial_value,
		       tBool       has_initial_value,
		       tTime       first_edge,
		       tTime       low_duration,
		       tTime       high_duration);

/* Allow a clock definition to be altered (overridden from the UI, etc.)
 *
 * Returns BK_ERROR on error, BK_SUCCESS on success.
 */
tStatus bk_alter_clock(tSimStateHdl simHdl,
		       tClock      handle,
		       tClockValue initial_value,
		       tBool       has_initial_value,
		       tTime       first_edge,
		       tTime       high_duration,
		       tTime       low_duration);

/* Associate a callback function with an event type for a particular
 * clock.
 *
 *   handle               - the handle of the clock
 *   on_edge_callback     - the function to call when the edge event occurs
 *   after_edge_callback  - the function to call after the edge event
 *   dir                  - direction of the clock edge
 *
 * Returns BK_ERROR on error, BK_SUCCESS on success.
 */
tStatus bk_set_clock_event_fn(tSimStateHdl simHdl,
			      tClock handle,
			      tScheduleFn on_edge_callback,
			      tScheduleFn after_edge_callback,
			      tEdgeDirection dir);

/* Trigger a clock edge at a given simulation time.
 * This function is for use with clocks that have no defined
 * waveform (ie. high_duration == low_duration == 0).
 *
 * Returns BK_ERROR on error, or the number of events scheduled
 * for the clock edge on success.
 */
tStatus bk_trigger_clock_edge(tSimStateHdl simHdl,
			      tClock handle, tEdgeDirection dir, tTime at);

/* Enqueue an initial clock edge (at time 0).
 * This function is for use with clocks that have no defined
 * waveform (ie. high_duration == low_duration == 0).
 *
 * Returns BK_ERROR on error, or the number of events scheduled for the
 * clock edge on success.
 */
tStatus bk_enqueue_initial_clock_edge(tSimStateHdl simHdl,
				      tClock handle, tEdgeDirection dir);

/* Get the clock handle associated with a clock domain name.
 *
 * Returns the clock handle for the domain, or BAD_CLOCK_HANDLE
 * if there is no clock domain with the given name.
 */
tClock bk_get_clock_by_name(tSimStateHdl simHdl, const char* name);

/* If there is already a clock domain with the given name,
 * return the handle for it.  If there is no clock domain with
 * this name yet, then create one and return the handle of the
 * new domain.  The domain characteristics can be set with
 * a subsequent call to bk_alter_clock().
 */
tClock bk_get_or_define_clock(tSimStateHdl simHdl, const char* name);

/* Get the number of clocks defined in the kernel */
tUInt32 bk_num_clocks(tSimStateHdl simHdl);

/* Get the clock handle for the nth clock.
 *
 * Returns the clock handle on success or BAD_CLOCK_HANDLE on error.
 */
tClock bk_get_nth_clock(tSimStateHdl simHdl, tUInt32 n);

/* Get various information for a clock */
const char* bk_clock_name(tSimStateHdl simHdl, tClock handle);
tClockValue bk_clock_initial_value(tSimStateHdl simHdl, tClock handle);
tTime bk_clock_first_edge(tSimStateHdl simHdl, tClock handle);
tTime bk_clock_duration(tSimStateHdl simHdl, tClock handle, tClockValue value);
tClockValue bk_clock_val(tSimStateHdl simHdl, tClock handle);
tUInt64 bk_clock_cycle_count(tSimStateHdl simHdl, tClock handle);
tUInt64 bk_clock_edge_count(tSimStateHdl simHdl,
			    tClock handle, tEdgeDirection dir);
tUInt32 bk_clock_vcd_num(tSimStateHdl simHdl, tClock handle);

/*
 * Setup a default reset waveform (asserted at time 0, deasserted at time 2).
 * This should be called before the first bk_advance() call.
 */
void bk_use_default_reset(tSimStateHdl simHdl);

/*
 * Simulation control
 */

/* Get the current simulation time */
tTime bk_now(tSimStateHdl simHdl);

/* Set simulation timescale - reporting scale factor and time unit for VCDs.
 *
 * Returns BK_ERROR on error, BK_SUCCESS on success.
 *
 * Errors include passing an invalid timescale unit and setting the timescale
 * after the beginning of the simulation.
 */
tStatus bk_set_timescale(tSimStateHdl simHdl, const char* scale_unit, tTime scale_factor);

/* Test if a given simulation time is still ongoing.
 * WARNING: This is a specialized function for use by
 * Bluesim primitives to facilitate connections to
 * event-driven simulation.  FOR EXPERT USE ONLY!
 */
tBool bk_is_same_time(tSimStateHdl simHdl, tTime t);

/* Test if we are currently executing within a combinational
 * schedule.  FOR EXPERT USE ONLY!
 */
tBool bk_is_combo_sched(tSimStateHdl simHdl);

/* Get information on the clock event queue */
tTime bk_clock_last_edge(tSimStateHdl simHdl, tClock handle);
tTime bk_clock_combinational_time(tSimStateHdl simHdl, tClock handle);

/* Quit simulation at the end of the current time slice. */
void bk_quit_at(tSimStateHdl simHdl, tTime t);

/* Quit simulation at the end of the given time slice.
 *
 * Returns BK_ERROR on error and BK_SUCCESS on success.
 */
tStatus bk_quit_after_edge(tSimStateHdl simHdl,
			   tClock handle, tEdgeDirection dir, tUInt64 cycle);

/* Execute simulation events until none remain, simulation is
 * interrupted, or a stopping condition (time limit, etc.) is
 * encountered.
 *
 * When called with an argument of 0, it will not return until
 * the simulation has completed.  When called with a non-zero
 * argument it will return immediately, and bk_sync() and
 * bk_is_running() should be used to synchronize with the simulation
 * thread.
 *
 * Returns BK_ERROR on error and BK_SUCCESS on success.
 */
tStatus bk_advance(tSimStateHdl simHdl, tBool async);

/* Test if the simulation thread is still running.
 *
 * Returns 0 if the thread is not running and non-zero if
 * the thread is running.
 */
tBool bk_is_running(tSimStateHdl simHdl);

/* Wait for a simulation started using bk_advance in async mode
 * to complete.
 *
 * Returns the simulation time at which execution stopped.
 */
tTime bk_sync(tSimStateHdl simHdl);

/* Schedule a UI callback for the end of a given timeslice,
 * unless there is already one scheduled at that time.
 *
 * Returns BK_ERROR on error or BK_SUCCESS on success.
 */
tStatus bk_schedule_ui_event(tSimStateHdl simHdl, tTime at);

/* Remove a UI callback previously scheduled at the end of a given timeslice.
 *
 * Returns BK_ERROR on error or BK_SUCCESS on success.
 */
tStatus bk_remove_ui_event(tSimStateHdl simHdl, tTime at);

/*
 * Routines to control debugging functionality.
 */

void bk_enable_state_dumping(tSimStateHdl simHdl);
void bk_disable_state_dumping(tSimStateHdl simHdl);
tBool bk_is_state_dumping_enabled(tSimStateHdl simHdl);
void bk_dump_state(tSimStateHdl simHdl, const char* label);

void bk_enable_cycle_dumping(tSimStateHdl simHdl);
void bk_disable_cycle_dumping(tSimStateHdl simHdl);
tBool bk_is_cycle_dumping_enabled(tSimStateHdl simHdl);
void bk_dump_cycle_counts(tSimStateHdl simHdl,
			  const char* label, tClock handle);

tBool bk_enable_VCD_dumping(tSimStateHdl simHdl);
void bk_disable_VCD_dumping(tSimStateHdl simHdl);
tBool bk_is_VCD_dumping_enabled(tSimStateHdl simHdl);
void bk_VCD_combo_update(tSimStateHdl simHdl, tTime t);

/* VCD control routines */
tStatus bk_set_VCD_file(tSimStateHdl simHdl, const char* name);
const char* bk_get_VCD_file_name(tSimStateHdl simHdl);
void bk_set_VCD_depth(tSimStateHdl simHdl, tUInt32 depth);
tStatus bk_VCD_checkpoint(tSimStateHdl simHdl);
void bk_set_VCD_filesize_limit(tSimStateHdl simHdl, tUInt64 bytes);
void bk_flush_VCD_output(tSimStateHdl simHdl);

/* Call to enable clock edges without logic (for interactive stepping) */
void bk_set_interactive(tSimStateHdl simHdl);

/*
 * Callbacks to stop simulation within a schedule or model.
 */

/* Pause the simulation and return to the UI at the end of this
 * simulation cycle.  The status value is made available to
 * callers of bk_exit_status().
 */
void bk_stop_now(tSimStateHdl simHdl, tSInt32 status);

/* Abort the simulation and return to the UI at the end of this
 * simulation cycle.  The status value is made available to
 * callers of bk_exit_status().
 */
void bk_finish_now(tSimStateHdl simHdl, tSInt32 status);

/* Test if $stop was called. */
tBool bk_stopped(tSimStateHdl simHdl);

/* Test if $finish was called. */
tBool bk_finished(tSimStateHdl simHdl);

/* Retrieve the status value of the last call to bk_stop_now()
 * or bk_finish_now().
 */
tSInt32 bk_exit_status(tSimStateHdl simHdl);


/*
 * Callbacks to stop simulation from outside a schedule or model.
 */

/* Abort the simulation and return to the UI at the end of the
 * current simulation cycle.
 */
void bk_abort_now(tSimStateHdl simHdl);

/* Test if bk_abort_now() was called. */
tBool bk_aborted(tSimStateHdl simHdl);


/*
 * Routines for setting and testing arguments (eg., plusargs).
 */

/* Add an argument string */
void bk_append_argument(tSimStateHdl simHdl, const char* arg);

/* Retrieve the trailing portion of the first matching argument */
const char* bk_match_argument(tSimStateHdl simHdl, const char* name);

/* Routine which provides direct access to the top-level model.  This
 * should only be used by callers that know exactly what they are doing.
 */
void* bk_get_model_instance(tSimStateHdl simHdl);

/*
 * API routines for finding and working with symbols
 */

/* Get the symbol for the top module. */
tSymbol bk_top_symbol(tSimStateHdl simHdl);

/* Lookup a symbol by name.  Returns BAD_SYMBOL if the named
 * symbol is not found.
 */
tSymbol bk_lookup_symbol(tSymbol root, const char* name);

/* Get the key for a symbol */
const char* bk_get_key(tSymbol sym);

/* Test if a symbol represents a module */
tBool bk_is_module(tSymbol sym);

/* Test if a symbol represents a rule */
tBool bk_is_rule(tSymbol sym);

/* Test if a symbol represents a value */
tBool bk_is_single_value(tSymbol sym);

/* Test if a symbol represents a range of values */
tBool bk_is_value_range(tSymbol sym);

/* Get the size for a symbol (for value and value range symbols) */
tUInt32 bk_get_size(tSymbol sym);

/* Get the value for a symbol (as a void*) */
void* bk_get_ptr(tSymbol sym);

/* Get a pointer to the value for a value symbol.
 * Returns NULL for other symbol types.
 */
const unsigned int* bk_peek_symbol_value(tSymbol sym);

/* Get the minimum address for a value range.
 * Returns NULL for other symbol types.
 */
tUInt64 bk_get_range_min_addr(tSymbol sym);

/* Get the maximum address for a value range.
 * Returns NULL for other symbol types.
 */
tUInt64 bk_get_range_max_addr(tSymbol sym);

/* Get a pointer to a value selected from a range.
 * Returns NULL for other symbol types, or if the address is out of bounds.
 */
const unsigned int* bk_peek_range_value(tSymbol sym, tUInt64 addr);

/* Get the number of sub-symbols of a module.
 * Returns 0 for other symbol types.
 */
tUInt32 bk_num_symbols(tSymbol sym);

/* Get the Nth sub-symbol of a module (starting at 0).
 * Returns BAD_SYMBOL for other symbol types.
 */
tSymbol bk_get_nth_symbol(tSymbol sym, tUInt32 n);

#if __cplusplus
} /* extern "C" */
#endif

#endif /* __BLUESIM_KERNEL_H__ */
