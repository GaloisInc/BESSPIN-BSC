#include "alt_types.h"
#include "io.h"
#include "system.h"
#include "sequencer.h"
#include "tcldbg.h"
#include "sequencer_auto.h"


#define NEWVERSION_RDDESKEW 1
#define NEWVERSION_WRDESKEW 1
#define NEWVERSION_GW 1
#define NEWVERSION_WL 1
#define NEWVERSION_DQSEN 1

#if HALF_RATE
#define HALF_RATE_MODE 1
#else
#define HALF_RATE_MODE 0
#endif


#define DYNAMIC_CALIB_STEPS (dyn_calib_steps)

#if STATIC_SIM_FILESET
#define STATIC_IN_RTL_SIM CALIB_IN_RTL_SIM
#else
#define STATIC_IN_RTL_SIM 0
#endif

#if STATIC_SKIP_MEM_INIT
#define STATIC_SKIP_DELAY_LOOPS CALIB_SKIP_DELAY_LOOPS
#else
#define STATIC_SKIP_DELAY_LOOPS 0
#endif

#if STATIC_FULL_CALIBRATION
#define STATIC_CALIB_STEPS (STATIC_IN_RTL_SIM | CALIB_SKIP_FULL_TEST | STATIC_SKIP_DELAY_LOOPS)
#elif STATIC_QUICK_CALIBRATION
#define STATIC_CALIB_STEPS (STATIC_IN_RTL_SIM | CALIB_SKIP_FULL_TEST | CALIB_SKIP_WRITES | CALIB_SKIP_DELAY_SWEEPS | CALIB_SKIP_ALL_BITS_CHK | STATIC_SKIP_DELAY_LOOPS)
#elif STATIC_SKIP_CALIBRATION
#define STATIC_CALIB_STEPS (STATIC_IN_RTL_SIM | CALIB_SKIP_FULL_TEST | CALIB_SKIP_WRITES | CALIB_SKIP_WLEVEL | CALIB_SKIP_LFIFO | CALIB_SKIP_VFIFO | CALIB_SKIP_DELAY_SWEEPS | CALIB_SKIP_ALL_BITS_CHK | STATIC_SKIP_DELAY_LOOPS)
#else
#undef STATIC_CALIB_STEPS
#endif

alt_u16 dyn_calib_steps;

#define DYNAMIC_CALIB_STEPS (dyn_calib_steps)

alt_u16 skip_delay_mask;
alt_u16 skip_delay_val;

#define SKIP_DELAY_LOOP_VALUE_OR_ZERO(non_skip_value) \
	((non_skip_value) & skip_delay_mask)


gbl_t *gbl;
param_t *param;

void initialize(void)
{
	alt_u32 i;

	//USER calibration has control over path to memory 

	IOWR_32DIRECT (PHY_MGR_MUX_SEL, 0, 1);

	//USER memory clock is not stable we begin initialization 

	IOWR_32DIRECT (PHY_MGR_RESET_MEM_STBL, 0, 0);

	//USER calibration status all set to zero 

	IOWR_32DIRECT (PHY_MGR_CAL_STATUS, 0, 0);
	IOWR_32DIRECT (PHY_MGR_CAL_DEBUG_INFO, 0, 0);

	if (((DYNAMIC_CALIB_STEPS) & CALIB_SKIP_ALL) != CALIB_SKIP_ALL) {
		param->read_correct_mask_vg  = ((t_btfld)1 << (RW_MGR_MEM_DQ_PER_READ_DQS / RW_MGR_MEM_VIRTUAL_GROUPS_PER_READ_DQS)) - 1;
		param->write_correct_mask_vg = ((t_btfld)1 << (RW_MGR_MEM_DQ_PER_READ_DQS / RW_MGR_MEM_VIRTUAL_GROUPS_PER_READ_DQS)) - 1;
		param->read_correct_mask     = ((t_btfld)1 << RW_MGR_MEM_DQ_PER_READ_DQS) - 1;
		param->write_correct_mask    = ((t_btfld)1 << RW_MGR_MEM_DQ_PER_WRITE_DQS) - 1;
		param->dm_correct_mask       = ((t_btfld)1 << (RW_MGR_MEM_DATA_WIDTH / RW_MGR_MEM_DATA_MASK_WIDTH)) - 1;
	}

	//USER Only initialize rank and group mask when not in debug mode so that
	//USER we can set it externally
	if (!gbl->phy_in_debug_mode) {

		//USER fill array used to determine if we skip certain ranks 

		for (i = 0; i < RW_MGR_MEM_NUMBER_OF_RANKS; i++) {
			param->skip_ranks[i] = 0;
		}

		param->skip_groups = 0;
	}
}

//USER Zero all DQS config and IO config blocks. 
void scc_mgr_zero_all (void)
{
	alt_u32 i;

	//USER Zero all DQS config settings 

	for (i = 0; i < RW_MGR_MEM_IF_READ_DQS_WIDTH; i++) {
		WRITE_SCC_DQS_IN_DELAY(i, IO_DQS_IN_RESERVE);
		WRITE_SCC_DQS_EN_PHASE(i, 0);
		WRITE_SCC_DQS_EN_DELAY(i, 0);
		WRITE_SCC_DQDQS_OUT_PHASE(i, 0);
		WRITE_SCC_OCT_OUT1_DELAY(i, 0);
		WRITE_SCC_OCT_OUT2_DELAY(i, IO_DQS_OUT_RESERVE);
	}

	//USER multicast to all DQS group enables 

	IOWR_32DIRECT (SCC_MGR_DQS_ENA, 0, 0xff);
}

void scc_mgr_zero_group (alt_u32 group, alt_u32 test_begin)
{
	alt_u32 i;

	//USER Zero all DQ config settings 

	for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++)
	{
		WRITE_SCC_DQ_OUT1_DELAY(i, 0);
		WRITE_SCC_DQ_OUT2_DELAY(i, IO_DQ_OUT_RESERVE);
		WRITE_SCC_DQ_IN_DELAY(i, 0);
	}

	//USER multicast to all DQ enables 

	IOWR_32DIRECT (SCC_MGR_DQ_ENA, 0, 0xff);

	//USER Zero all DM config settings 

	for (i = 0; i < RW_MGR_NUM_DM_PER_WRITE_GROUP; i++)
	{
		WRITE_SCC_DM_IO_IN_DELAY(i, 0);
		WRITE_SCC_DM_IO_OUT1_DELAY(i, 0);
		WRITE_SCC_DM_IO_OUT2_DELAY(i, IO_DM_OUT_RESERVE);
	}

	IOWR_32DIRECT (SCC_MGR_DM_ENA, 0, 0xff);

	//USER zero all DQS io settings 

	WRITE_SCC_DQS_IO_IN_DELAY(0);
	WRITE_SCC_DQS_IO_OUT1_DELAY(0);
	WRITE_SCC_DQS_IO_OUT2_DELAY(IO_DQS_OUT_RESERVE);

	//USER multicast to all DQS IO enables 

	IOWR_32DIRECT (SCC_MGR_DQS_IO_ENA, 0, 0);

	//USER hit update to zero everything 

	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
}

//USER load up dqs config settings 

void scc_mgr_load_dqs (alt_u32 dqs)
{
	IOWR_32DIRECT (SCC_MGR_DQS_ENA, 0, dqs);
}

//USER load up dqs io config settings 

void scc_mgr_load_dqs_io ()
{
	IOWR_32DIRECT (SCC_MGR_DQS_IO_ENA, 0, 0);
}

//USER load up dq config settings 

void scc_mgr_load_dq (alt_u32 dq, alt_u32 dq_in_group)
{
	IOWR_32DIRECT (SCC_MGR_DQ_ENA, 0, dq_in_group);
}

//USER load up dm config settings 

void scc_mgr_load_dm (alt_u32 dm)
{
	IOWR_32DIRECT (SCC_MGR_DM_ENA, 0, dm);
}

//USER apply and load a particular input delay for the DQ pins in a group
//USER group_bgn is the index of the first dq pin (in the write group)

void scc_mgr_apply_group_dq_in_delay (alt_u32 group_bgn, alt_u32 delay)
{
	alt_u32 i, p;

	for (i = 0, p = group_bgn; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++, p++) {
		WRITE_SCC_DQ_IN_DELAY(p, delay);
		scc_mgr_load_dq (p, p);
	}
}

//USER apply and load a particular output delay for the DQ pins in a group

void scc_mgr_apply_group_dq_out1_delay (alt_u32 group_bgn, alt_u32 delay1)
{
	alt_u32 i, p;

	for (i = 0, p = group_bgn; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++, p++) {
		WRITE_SCC_DQ_OUT1_DELAY(i, delay1);
		scc_mgr_load_dq (p, i);
	}
}

void scc_mgr_apply_group_dq_out2_delay (alt_u32 group_bgn, alt_u32 delay2)
{
	alt_u32 i, p;

	for (i = 0, p = group_bgn; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++, p++) {
		WRITE_SCC_DQ_OUT2_DELAY(i, delay2);
		scc_mgr_load_dq (p, i);
	}
}

//USER apply and load a particular output delay for the DM pins in a group

void scc_mgr_apply_group_dm_out1_delay (alt_u32 delay1)
{
	alt_u32 i;

	for (i = 0; i < RW_MGR_NUM_DM_PER_WRITE_GROUP; i++) {
		WRITE_SCC_DM_IO_OUT1_DELAY(i, delay1);
		scc_mgr_load_dm (i);
	}
}


//USER apply and load delay on both DQS and OCT out1
void scc_mgr_apply_group_dqs_io_and_oct_out1 (alt_u32 write_group, alt_u32 delay)
{
	WRITE_SCC_DQS_IO_OUT1_DELAY(delay);
	scc_mgr_load_dqs_io ();

	WRITE_SCC_OCT_OUT1_DELAY(write_group, delay);
	scc_mgr_load_dqs (write_group);
}

//USER apply a delay to the entire output side: DQ, DM, DQS, OCT 

void scc_mgr_apply_group_all_out_delay (alt_u32 write_group, alt_u32 group_bgn, alt_u32 delay)
{
	alt_u32 i;

	//USER dq shift 

	scc_mgr_apply_group_dq_out1_delay (group_bgn, delay);

	//USER dm shift 

	scc_mgr_apply_group_dm_out1_delay (delay);

	//USER dqs and oct shift 

	scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, delay);
}

//USER apply a delay to the entire output side: DQ, DM, DQS, OCT 

void scc_mgr_apply_group_all_out_delay_add (alt_u32 write_group, alt_u32 group_bgn, alt_u32 delay)
{
	alt_u32 i, p, new_delay;

	//USER dq shift 

	for (i = 0, p = group_bgn; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++, p++) {

		new_delay = READ_SCC_DQ_OUT2_DELAY(i);
		new_delay += delay;

		if (new_delay > IO_IO_OUT2_DELAY_MAX) {
			new_delay = IO_IO_OUT2_DELAY_MAX;
		}

		WRITE_SCC_DQ_OUT2_DELAY(i, new_delay);
		scc_mgr_load_dq (p, i);
	}

	//USER dm shift 

	for (i = 0; i < RW_MGR_NUM_DM_PER_WRITE_GROUP; i++) {
		new_delay = READ_SCC_DM_IO_OUT2_DELAY(i);
		new_delay += delay;

		if (new_delay > IO_IO_OUT2_DELAY_MAX) {
			new_delay = IO_IO_OUT2_DELAY_MAX;
		}

		WRITE_SCC_DM_IO_OUT2_DELAY(i, new_delay);
		scc_mgr_load_dm (i);
	}

	//USER dqs shift 

	new_delay = READ_SCC_DQS_IO_OUT2_DELAY();
	new_delay += delay;

	if (new_delay > IO_IO_OUT2_DELAY_MAX) {
		new_delay = IO_IO_OUT2_DELAY_MAX;
	}

	WRITE_SCC_DQS_IO_OUT2_DELAY(new_delay);
	scc_mgr_load_dqs_io ();

	//USER oct shift 

	new_delay = READ_SCC_OCT_OUT2_DELAY(write_group);
	new_delay += delay;

	if (new_delay > IO_IO_OUT2_DELAY_MAX) {
		new_delay = IO_IO_OUT2_DELAY_MAX;
	}

	WRITE_SCC_OCT_OUT2_DELAY(write_group, new_delay);
	scc_mgr_load_dqs (write_group);
}

#if DDR3
void set_rank_and_odt_mask(alt_u32 rank, alt_u32 odt_mode)
{
	alt_u32 odt_mask_0 = 0;
	alt_u32 odt_mask_1 = 0;
	alt_u32 cs_and_odt_mask;

	if(odt_mode == RW_MGR_ODT_MODE_READ_WRITE)
	{
		if(RW_MGR_MEM_NUMBER_OF_RANKS == 1) { 
			//USER 1 Rank
			//USER Read: ODT = 0
			//USER Write: ODT = 1
			odt_mask_0 = 0x0;
			odt_mask_1 = 0x1;
		} else if(RW_MGR_MEM_NUMBER_OF_RANKS == 2) { 
			//USER 2 Ranks
			if(RW_MGR_MEM_NUMBER_OF_CS_PER_DIMM == 1) {
				//USER - Dual-Slot , Single-Rank (1 chip-select per DIMM)
				//USER Read: Turn on ODT on the opposite rank
				//USER Write: Turn on ODT on all ranks
				odt_mask_0 = 0x3 & ~(1 << rank);
				odt_mask_1 = 0x3;
			} else {
				//USER - Single-Slot , Dual-rank DIMMs (2 chip-selects per DIMM)
				//USER Read: Turn on ODT off on all ranks
				//USER Write: Turn on ODT on active rank
				odt_mask_0 = 0x0;
				odt_mask_1 = 0x3 & (1 << rank);
			}
		} else { 
			//USER 4 Ranks
			//USER Read:
			//USER ----------+-----------------------+
			//USER           |                       |
			//USER           |         ODT           |
			//USER Read From +-----------------------+
			//USER   Rank    |  3  |  2  |  1  |  0  |
			//USER ----------+-----+-----+-----+-----+
			//USER     0     |  0  |  1  |  0  |  0  |
			//USER     1     |  1  |  0  |  0  |  0  |
			//USER     2     |  0  |  0  |  0  |  1  |
			//USER     3     |  0  |  0  |  1  |  0  |
			//USER ----------+-----+-----+-----+-----+
			//USER
			//USER Write:
			//USER ----------+-----------------------+
			//USER           |                       |
			//USER           |         ODT           |
			//USER Write To  +-----------------------+
			//USER   Rank    |  3  |  2  |  1  |  0  |
			//USER ----------+-----+-----+-----+-----+
			//USER     0     |  0  |  1  |  0  |  1  |
			//USER     1     |  1  |  0  |  1  |  0  |
			//USER     2     |  0  |  1  |  0  |  1  |
			//USER     3     |  1  |  0  |  1  |  0  |
			//USER ----------+-----+-----+-----+-----+
			switch(rank)
			{
				case 0:
					odt_mask_0 = 0x4;
					odt_mask_1 = 0x5;
				break;
				case 1:
					odt_mask_0 = 0x8;
					odt_mask_1 = 0xA;
				break;
				case 2:
					odt_mask_0 = 0x1;
					odt_mask_1 = 0x5;
				break;
				case 3:
					odt_mask_0 = 0x2;
					odt_mask_1 = 0xA;
				break;
			}
		}
	}
	else
	{
		odt_mask_0 = 0x0;
		odt_mask_1 = 0x0;
	}

	cs_and_odt_mask = 
		(0xFF & ~(1 << rank)) |
		((0xFF & odt_mask_0) << 8) |
		((0xFF & odt_mask_1) << 16);

	IOWR_32DIRECT (RW_MGR_SET_CS_AND_ODT_MASK, 0, cs_and_odt_mask);
}
#else
#if DDR2
void set_rank_and_odt_mask(alt_u32 rank, alt_u32 odt_mode)
{
	alt_u32 odt_mask_0 = 0;
	alt_u32 odt_mask_1 = 0;
	alt_u32 cs_and_odt_mask;

	if(odt_mode == RW_MGR_ODT_MODE_READ_WRITE)
	{
		if(RW_MGR_MEM_NUMBER_OF_RANKS == 1) { 
			//USER 1 Rank
			//USER Read: ODT = 0
			//USER Write: ODT = 1
			odt_mask_0 = 0x0;
			odt_mask_1 = 0x1;
		} else if(RW_MGR_MEM_NUMBER_OF_RANKS == 2) { 
			//USER 2 Ranks
			if(RW_MGR_MEM_NUMBER_OF_CS_PER_DIMM == 1) {
				//USER - Dual-Slot , Single-Rank (1 chip-select per DIMM)
				//USER Read/Write: Turn on ODT on the opposite rank
				odt_mask_0 = 0x3 & ~(1 << rank);
				odt_mask_1 = 0x3 & ~(1 << rank);
			} else {
				//USER - Single-Slot , Dual-rank DIMMs (2 chip-selects per DIMM)
				//USER Read: Turn on ODT off on all ranks
				//USER Write: Turn on ODT on active rank
				odt_mask_0 = 0x0;
				odt_mask_1 = 0x3 & (1 << rank);
			}
		} else { 
			//USER 4 Ranks
			//USER Read/Write:
			//USER -----------+-----------------------+
			//USER            |                       |
			//USER            |         ODT           |
			//USER Read/Write |                       |
			//USER   From     +-----------------------+
			//USER   Rank     |  3  |  2  |  1  |  0  |
			//USER -----------+-----+-----+-----+-----+
			//USER     0      |  0  |  1  |  0  |  0  |
			//USER     1      |  1  |  0  |  0  |  0  |
			//USER     2      |  0  |  0  |  0  |  1  |
			//USER     3      |  0  |  0  |  1  |  0  |
			//USER -----------+-----+-----+-----+-----+
			switch(rank)
			{
				case 0:
					odt_mask_0 = 0x4;
					odt_mask_1 = 0x4;
				break;
				case 1:
					odt_mask_0 = 0x8;
					odt_mask_1 = 0x8;
				break;
				case 2:
					odt_mask_0 = 0x1;
					odt_mask_1 = 0x1;
				break;
				case 3:
					odt_mask_0 = 0x2;
					odt_mask_1 = 0x2;
				break;
			}
		}
	}
	else
	{
		odt_mask_0 = 0x0;
		odt_mask_1 = 0x0;
	}

	cs_and_odt_mask = 
		(0xFF & ~(1 << rank)) |
		((0xFF & odt_mask_0) << 8) |
		((0xFF & odt_mask_1) << 16);

	IOWR_32DIRECT (RW_MGR_SET_CS_AND_ODT_MASK, 0, cs_and_odt_mask);
}
#else // QDRII and RLDRAMII
void set_rank_and_odt_mask(alt_u32 rank, alt_u32 odt_mode)
{
	alt_u32 cs_and_odt_mask = 
		(0xFF & ~(1 << rank));

	IOWR_32DIRECT (RW_MGR_SET_CS_AND_ODT_MASK, 0, cs_and_odt_mask);
}
#endif
#endif

#if RDIMM && DDR3
void rw_mgr_rdimm_initialize(void)
{
	alt_u32 i;
	const alt_u32 AC_BASE_CONTENT = __RW_MGR_CONTENT_ac_rdimm;
	//USER These values should be dynamically loaded instead of hard-coded
	const alt_u32 AC_ADDRESS_POSITION = 0x0;
	const alt_u32 AC_BANK_ADDRESS_POSITION = 0xD;
	alt_u32 conf_word;
	alt_u32 ac_content;

	//USER RDIMM registers are programmed by writing 16 configuration words
	//USER 1. An RDIMM command is a NOP with all CS asserted
	//USER 2. The 4-bit address of the configuration words is 
	//USER    * { mem_ba[2] , mem_a[2] , mem_a[1] , mem_a[0] }
	//USER 3. The 4-bit configuration word is
	//USER    * { mem_ba[1] , mem_ba[0] , mem_a[4] , mem_a[3] }

	//USER Turn on all ranks
	IOWR_32DIRECT (RW_MGR_SET_CS_AND_ODT_MASK, 0, RW_MGR_RANK_ALL);

	for(i = 0; i < 16; i++)
	{
		if(i < 8)
		{
			conf_word = (RDIMM_CONFIG_WORD_LOW >> (i * 4)) & 0xF;
		}
		else
		{
			conf_word = (RDIMM_CONFIG_WORD_HIGH >> ((i - 8) * 4)) & 0xF;
		}

		ac_content = 
			AC_BASE_CONTENT | 
			//USER Word address
			((i & 0x7) << AC_ADDRESS_POSITION) |
			(((i >> 3) & 0x1) << (AC_BANK_ADDRESS_POSITION + 2)) |
			//USER Configuration Word
			(((conf_word >> 2) & 0x3) << (AC_BANK_ADDRESS_POSITION)) |
			((conf_word & 0x3) << (AC_ADDRESS_POSITION + 3));

		//USER Override the AC row with the RDIMM command
		IOWR_32DIRECT(BASE_RW_MGR, 0x1C00 + (__RW_MGR_ac_rdimm << 2), ac_content);

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_RDIMM_CMD);
	
		//USER When sending the RC2 word, tSTAB time must elapse before the next command
		//USER is sent out
		//USER Right now I'm hard-coding 6us as it was in the legacy sequencer and specified in
		//USER a Samsung device I found on the web. Eventually we need something more robust...
		if(i == 2) 
		{
			//USER tSTAB = 6 us @ 266MHz (3.75 ns) = ~ 1600
			
			//USER If 'a' is the number of cycles in the outer loop and 'b' in the inner loop
			//USER it takes the following number of cycles to complete the operation:
			//USER number_of_cycles = ((2 + n) * b + 2) * a
			//USER where n is the number of instructions in the inner loop
			//USER One possible solution is n = 0 , a = 7 , b = 114 => a = 0x07, b = 0x72

			//USER Load counters
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x07));
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x72));

			//USER Load jump address
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_LOOP_2_out);
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_LOOP_2_in);
		
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_LOOP_2_out);
		}
	}
}
#else
void rw_mgr_rdimm_initialize(void) { }
#endif

#if DDR3
void rw_mgr_mem_initialize (void)
{
	alt_u32 r;

	//USER The reset / cke part of initialization is broadcasted to all ranks
	IOWR_32DIRECT (RW_MGR_SET_CS_AND_ODT_MASK, 0, RW_MGR_RANK_ALL);

	//USER Counters are located @ 0x800
	//USER Jump address are located @ 0xC00
	//USER For both, registers 0 to 3 are selected using bits 3 and 2, like in
	//USER 0x800, 0x804, 0x808, 0x80C and 0xC00, 0xC04, 0xC08, 0xC0C
	
	//USER start with memory RESET activated

	//USER tINIT = 200us

	//USER 200us @ 266MHz (3.75 ns) ~ 54000 clock cycles
	//USER If a and b are the number of iteration in 2 nested loops
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = ((2 + n) * a + 2) * b
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 0 , a = 256 , b = 106 => a = FF, b = 6A

	//USER Load counters
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0xFF));
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x6A));
	
	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_INIT_RESET_0_CKE_0);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_INIT_RESET_0_CKE_0_inloop);

	//USER Execute count instruction
	//USER IOWR_32DIRECT (BASE_RW_MGR, 0, __RW_MGR_COUNT_REG_0);
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_INIT_RESET_0_CKE_0);

	//USER indicate that memory is stable
	IOWR_32DIRECT (PHY_MGR_RESET_MEM_STBL, 0, 1);

	//USER transition the RESET to high 
	//USER Wait for 500us

	//USER 500us @ 266MHz (3.75 ns) ~ 134000 clock cycles
	//USER If a and b are the number of iteration in 2 nested loops
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = ((2 + n) * a + 2) * b
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 2 , a = 131 , b = 256 => a = 83, b = FF

	//USER Load counters
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x83));
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0xFF));

	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_INIT_RESET_1_CKE_0);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_INIT_RESET_1_CKE_0_inloop_1);

	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_INIT_RESET_1_CKE_0);

	//USER bring up clock enable 

	//USER tXRP < 250 ck cycles
	//USER If a is the number of iteration in a loop
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = (2 + n) * a
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 0 , a = 126 => a = 7E
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x7E));

	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);

	//USER Execute IDLE loop
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);

	for (r = 0; r < RW_MGR_MEM_NUMBER_OF_RANKS; r++) {
		if (param->skip_ranks[r]) {
			//USER request to skip the rank

			continue;
		}

		set_rank_and_odt_mask(r, RW_MGR_ODT_MODE_OFF);

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_NOP);

		//USER Use Mirror-ed commands for odd ranks if address mirrorring is on
		if((RW_MGR_MEM_ADDRESS_MIRRORING >> r) & 0x1) {
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS2_MIRR);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS3_MIRR);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS1_MIRR);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS0_DLL_RESET_MIRR);
		} else {
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS2);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS3);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS1);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS0_DLL_RESET);
		}

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_ZQCL);

		//USER tZQinit = tDLLK = 512 ck cycles
		//USER If a is the number of iteration in a loop
		//USER it takes the following number of cycles to complete the operation:
		//USER number_of_cycles = (2 + n) * a
		//USER where n is the number of instruction in the inner loop
		//USER One possible solution is n = 0 , a = 256 => a = FF
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0xFF));

		//USER Load jump address
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);

		//USER Execute IDLE loop
		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);
	}
	rw_mgr_rdimm_initialize();
}
#endif // DDR3

#if DDR2
void rw_mgr_mem_initialize (void)
{
	alt_u32 r;

	//USER *** NOTE ***
	//USER The following STAGE (n) notation refers to the corresponding stage in the Micron datasheet

	//USER Counters are located @ 0x800
	//USER Jump address are located @ 0xC00
	//USER For both, registers 0 to 3 are selected using bits 3 and 2, like in
	//USER 0x800, 0x804, 0x808, 0x80C and 0xC00, 0xC04, 0xC08, 0xC0C
	
	//USER *** STAGE (1, 2, 3) ***

	//USER start with CKE low 

	//USER tINIT = 200us

	//USER 200us @ 300MHz (3.33 ns) ~ 60000 clock cycles
	//USER If a and b are the number of iteration in 2 nested loops
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = ((2 + n) * b + 2) * a
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 0 , a = 256 , b = 118 => a = FF, b = 76

	//USER Load counters
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0xFF));
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x76));
	
	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_INIT_CKE_0);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_INIT_CKE_0_inloop);

	//USER Execute count instruction
	//USER IOWR_32DIRECT (BASE_RW_MGR, 0, __RW_MGR_COUNT_REG_0);
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_INIT_CKE_0);

	//USER indicate that memory is stable 
	IOWR_32DIRECT (PHY_MGR_RESET_MEM_STBL, 0, 1);

	//USER Bring up CKE 
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_NOP);

	//USER *** STAGE (4)

	//USER Wait for 400ns 

	//USER 400ns @ 300MHz (3.333 ns) ~ 120 cycles
	//USER If a is the number of iteration in a loop
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = (2 + n) * a
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 0 , a = 60 => a = 0x40
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x40));

	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);

	//USER Execute IDLE loop
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);

	//USER Multi-rank section begins here
	for (r = 0; r < RW_MGR_MEM_NUMBER_OF_RANKS; r++) {
		if (param->skip_ranks[r]) {
			//USER request to skip the rank

			continue;
		}

	set_rank_and_odt_mask(r, RW_MGR_ODT_MODE_OFF);

	//USER * **** *
	//USER * NOTE *
	//USER * **** *
	//USER The following commands must be spaced by tMRD or tRPA which are in the order
	//USER of 2 to 4 full rate cycles. This is peanuts in the NIOS domain, so for now
	//USER we can avoid redundant wait loops

	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_PRECHARGE_ALL);

	//USER *** STAGE (5)
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_EMR2);

	//USER *** STAGE (6)
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_EMR3);

	//USER *** STAGE (7)
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_EMR);

	//USER *** STAGE (8)
	//USER DLL reset
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MR_DLL_RESET);

	//USER *** STAGE (9)
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_PRECHARGE_ALL);

	//USER *** STAGE (10)

	//USER Issue 2 refresh commands spaced by tREF 

	//USER First REFRESH
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_REFRESH);

	//USER tREF = 200ns @ 300MHz (3.333 ns) ~ 60 cycles
	//USER If a is the number of iteration in a loop
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = (2 + n) * a
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 0 , a = 30 => a = 0x20
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x20));

	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);

	//USER First idle loop
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);

	//USER Second REFRESH
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_REFRESH);

	//USER Second idle loop
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);

	//USER *** STAGE (11)
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MR_CALIB);

	//USER *** STAGE (12)
	//USER OCD defaults
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_EMR_OCD_ENABLE);

	//USER *** STAGE (13)
	//USER OCD exit
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_EMR);

	//USER *** STAGE (14)

	//USER The memory is now initialized. Before being able to use it, we must still
	//USER wait for the DLL to lock, 200 clock cycles after it was reset @ STAGE (8).
	//USER Since we cannot keep track of time in any other way, let's start counting from now


	//USER If a is the number of iteration in a loop
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = (2 + n) * a
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 0 , a = 100 => a = 64
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x64));

	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);

	//USER Execute IDLE loop
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);
	}
}
#endif // DDR2 

#if QDRII
void rw_mgr_mem_initialize (void)
{
	//USER Counters are located @ 0x800
	//USER Jump address are located @ 0xC00
	//USER For both, registers 0 to 3 are selected using bits 3 and 2, like in
	//USER 0x800, 0x804, 0x808, 0x80C and 0xC00, 0xC04, 0xC08, 0xC0C

	//USER Provide stable power and clock for 2048 cycles
	//USER 2048 memory cycles = 1024 (HR) or 2048 (FR) AFI cycles
	//USER If a and b are the number of iteration in 2 nested loops
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = ((2 + n) * a + 2) * b
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution for HR is n = 0 , a = 1 , b = 256 => a = 01, b = FF
	//USER One possible solution for FR is n = 0 , a = 3 , b = 256 => a = 03, b = FF

	//USER Load counters
#if HALF_RATE
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x01));
#else
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x03));
#endif
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0xFF));

	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_IDLE_LOOP_inloop);

	//USER Execute count instruction
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);

	//USER indicate that memory is stable
	IOWR_32DIRECT (PHY_MGR_RESET_MEM_STBL, 0, 1);
}

void rw_mgr_mem_dll_lock_wait (void)
{
	//USER The DLL in QDR requires 2048 clock cycles to lock

	//USER Counters are located @ 0x800
	//USER Jump address are located @ 0xC00
	//USER For both, registers 0 to 3 are selected using bits 3 and 2, like in
	//USER 0x800, 0x804, 0x808, 0x80C and 0xC00, 0xC04, 0xC08, 0xC0C

	//USER Provide stable power and clock for 2048 cycles
	//USER 2048 memory cycles = 1024 (HR) or 2048 (FR) AFI cycles
	//USER If a and b are the number of iteration in 2 nested loops
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = ((2 + n) * a + 2) * b
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution for HR is n = 0 , a = 1 , b = 256 => a = 01, b = FF
	//USER One possible solution for FR is n = 0 , a = 3 , b = 256 => a = 03, b = FF

	//USER Load counters
#if HALF_RATE
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x01));
#else
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x03));
#endif
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0xFF));

	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_IDLE_LOOP_inloop);

	//USER Execute count instruction
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);
}

#endif

#if RLDRAMII
void rw_mgr_mem_initialize (void)
{
	//USER Counters are located @ 0x800
	//USER Jump address are located @ 0xC00
	//USER For both, registers 0 to 3 are selected using bits 3 and 2, like in
	//USER 0x800, 0x804, 0x808, 0x80C and 0xC00, 0xC04, 0xC08, 0xC0C
	
	//USER start with memory RESET activated

	//USER tINIT = 200us

	//USER 200us @ 200 (5 ns) ~ 40000 clock cycles
	//USER If a and b are the number of iteration in 2 nested loops
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = ((2 + n) * a + 2) * b
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 0 , a = 256 , b = 78 => a = FF, b = 4E

	//USER Load counters
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0xFF));
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x4E));
	
	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_IDLE_LOOP_inloop);

	//USER Execute count instruction
	//USER IOWR_32DIRECT (BASE_RW_MGR, 0, __RW_MGR_COUNT_REG_0);
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);

	//USER indicate that memory is stable 
	IOWR_32DIRECT (PHY_MGR_RESET_MEM_STBL, 0, 1);

	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS_x3);

	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_REF_X8);

	//USER 1024 memory cycles = 2048 AFI 512 ck cycles
	//USER If a and b are the number of iteration in 2 nested loops
	//USER it takes the following number of cycles to complete the operation:
	//USER number_of_cycles = ((2 + n) * a + 2) * b
	//USER where n is the number of instruction in the inner loop
	//USER One possible solution is n = 0 , a = 3 , b = 256 => a = 03, b = FF

	//USER Load counters
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0x03));
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, SKIP_DELAY_LOOP_VALUE_OR_ZERO(0xFF));
	
	//USER Load jump address
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_IDLE_LOOP);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_IDLE_LOOP_inloop);

	//USER Execute count instruction
	//USER IOWR_32DIRECT (BASE_RW_MGR, 0, __RW_MGR_COUNT_REG_0);
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_IDLE_LOOP);
}
#endif

//USER  At the end of calibration we have to program the user settings in, and
//USER  hand off the memory to the user.

#if DDR3
void rw_mgr_mem_handoff (void)
{
	alt_u32 r;

	for (r = 0; r < RW_MGR_MEM_NUMBER_OF_RANKS; r++) {
		if (param->skip_ranks[r]) {
			//USER request to skip the rank

			continue;
		}

		set_rank_and_odt_mask(r, RW_MGR_ODT_MODE_OFF);

		//USER precharge all banks ... 

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_PRECHARGE_ALL);

		//USER load up MR settings specified by user 

		//USER Use Mirror-ed commands for odd ranks if address mirrorring is on
		if((RW_MGR_MEM_ADDRESS_MIRRORING >> r) & 0x1) {
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS2_MIRR);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS3_MIRR);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS1_MIRR);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS0_USER_MIRR);
		} else {
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS2);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS3);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS1);
			IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS0_USER);
		}

		//USER  need to wait tMOD (12CK or 15ns) time before issuing other commands,
		//USER  but we will have plenty of NIOS cycles before actual handoff so its okay.
	}
}
#endif // DDR3

#if DDR2
void rw_mgr_mem_handoff (void)
{
	alt_u32 r;

	for (r = 0; r < RW_MGR_MEM_NUMBER_OF_RANKS; r++) {
		if (param->skip_ranks[r]) {
			//USER request to skip the rank

			continue;
		}

		set_rank_and_odt_mask(r, RW_MGR_ODT_MODE_OFF);

		//USER precharge all banks ... 

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_PRECHARGE_ALL);

		//USER load up MR settings specified by user 

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_EMR2);

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_EMR3);

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_EMR);

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MR_USER);

		//USER need to wait tMOD (12CK or 15ns) time before issuing other commands,
		//USER but we will have plenty of NIOS cycles before actual handoff so its okay.
	}
}
#endif //USER DDR2

#if RLDRAMII
void rw_mgr_mem_handoff (void)
{
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_MRS);
}
#endif

#if QDRII
void rw_mgr_mem_handoff (void) {}
#endif

//USER load up the patterns we are going to use during a read test 
#if DDRX
void rw_mgr_mem_calibrate_read_load_patterns (void)
{
	alt_u32 r;

	for (r = 0; r < RW_MGR_MEM_NUMBER_OF_RANKS; r++) {
		if (param->skip_ranks[r]) {
			//USER request to skip the rank

			continue;
		}

		set_rank_and_odt_mask(r, RW_MGR_ODT_MODE_READ_WRITE);

		//USER Load up a constant bursts

		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x20);
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_GUARANTEED_WRITE_WAIT0);

		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, 0x20);
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_GUARANTEED_WRITE_WAIT1);

#if HALF_RATE
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_2, 0, 0x02);
#else
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_2, 0, 0x04);
#endif
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_GUARANTEED_WRITE_WAIT2);

#if HALF_RATE
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_3, 0, 0x02);
#else
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_3, 0, 0x04);
#endif
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_3, 0, __RW_MGR_GUARANTEED_WRITE_WAIT3);

		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_GUARANTEED_WRITE);
	}

	set_rank_and_odt_mask(0, RW_MGR_ODT_MODE_OFF);
}
#endif

#if QDRII
void rw_mgr_mem_calibrate_read_load_patterns (void)
{
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x20);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_GUARANTEED_WRITE_WAIT0);
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, 0x20);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_GUARANTEED_WRITE_WAIT1);
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_GUARANTEED_WRITE);
}
#endif

#if RLDRAMII
void rw_mgr_mem_calibrate_read_load_patterns (void)
{
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x20);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_GUARANTEED_WRITE_WAIT0);
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, 0x20);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_GUARANTEED_WRITE_WAIT1);
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_2, 0, 0x20);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_GUARANTEED_WRITE_WAIT2);
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_3, 0, 0x20);
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_3, 0, __RW_MGR_GUARANTEED_WRITE_WAIT3);
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_GUARANTEED_WRITE);
}
#endif

//USER  try a read and see if it returns correct data back. has dummy reads inserted into the mix
//USER  used to align dqs enable. has more thorough checks than the regular read test.

alt_u32 rw_mgr_mem_calibrate_read_test (alt_u32 group, alt_u32 num_tries, alt_u32 all_correct, t_btfld *bit_chk, alt_u32 all_groups)
{
	alt_u32 r, vg;
	t_btfld correct_mask_vg;
	t_btfld tmp_bit_chk;	

	*bit_chk = param->read_correct_mask;
	correct_mask_vg = param->read_correct_mask_vg;
	
	alt_u32 quick_read_mode = ((STATIC_CALIB_STEPS) & CALIB_SKIP_DELAY_SWEEPS) && ENABLE_SUPER_QUICK_CALIBRATION;
	for (r = 0; r < RW_MGR_MEM_NUMBER_OF_RANKS; r++) {
		if (param->skip_ranks[r]) {
			//USER request to skip the rank

			continue;
		}

		set_rank_and_odt_mask(r, RW_MGR_ODT_MODE_READ_WRITE);

		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, 0x10);
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_READ_B2B_WAIT1);
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_2, 0, 0x10);
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_READ_B2B_WAIT2);
		
		if(quick_read_mode) {
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x0);
		} else {
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x06);
		}
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_READ_B2B);
		if(quick_read_mode && !all_groups) {
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_3, 0, 0x0);
		} else {
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_3, 0, RW_MGR_MEM_IF_READ_DQS_WIDTH * RW_MGR_MEM_VIRTUAL_GROUPS_PER_READ_DQS - 1);
		}
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_3, 0, __RW_MGR_READ_B2B);
		
		tmp_bit_chk = 0;
		for (vg = 0; vg < RW_MGR_MEM_VIRTUAL_GROUPS_PER_READ_DQS; vg++)
		{
			//USER reset the fifos to get pointers to known state 

			IOWR_32DIRECT (PHY_MGR_CMD_FIFO_RESET, 0, 0);
			IOWR_32DIRECT (RW_MGR_RESET_READ_DATAPATH, 0, 0);	

			if (vg != 0) {
				tmp_bit_chk = tmp_bit_chk << (RW_MGR_MEM_DQ_PER_READ_DQS / RW_MGR_MEM_VIRTUAL_GROUPS_PER_READ_DQS);
			}

			IOWR_32DIRECT (all_groups ? RW_MGR_RUN_ALL_GROUPS : RW_MGR_RUN_SINGLE_GROUP, ((group*RW_MGR_MEM_VIRTUAL_GROUPS_PER_READ_DQS+vg) << 2), __RW_MGR_READ_B2B);
			tmp_bit_chk = tmp_bit_chk | (correct_mask_vg & ~(IORD_32DIRECT(BASE_RW_MGR, 0)));
		}
		*bit_chk &= tmp_bit_chk;
	}

	#if DDRX
	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, (group << 2), __RW_MGR_CLEAR_DQS_ENABLE);
	#endif
	
	if (all_correct)
	{
		set_rank_and_odt_mask(0, RW_MGR_ODT_MODE_OFF);
		return (*bit_chk == param->read_correct_mask);
	}
	else
	{
		set_rank_and_odt_mask(0, RW_MGR_ODT_MODE_OFF);
		return (*bit_chk != 0x00);
	}
}

alt_u32 rw_mgr_mem_calibrate_read_test_all_groups (void)
{
	t_btfld bit_chk;
	return rw_mgr_mem_calibrate_read_test (0, NUM_READ_TESTS, PASS_ALL_BITS, &bit_chk, 1);
}

void rw_mgr_decr_vfifo(alt_u32 grp, alt_u32 *v) {

	alt_u32 i;
	
	for (i = 0; i < VFIFO_SIZE-1; i++) {

		//USER fiddle with FIFO 
		if (!HALF_RATE_MODE || (*v & 1) == 1) {
			IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, grp);
		} else {
			IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, grp);
		}

		(*v)++;
	}
}

void rw_mgr_incr_vfifo(alt_u32 grp, alt_u32 *v) {

	//USER fiddle with FIFO 
	if (!HALF_RATE_MODE || (*v & 1) == 1) {
		IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, grp);
	} else {
		IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, grp);
	}

	(*v)++;
}

//USER find a good dqs enable to use 

#if QDRII || RLDRAMII
alt_u32 rw_mgr_mem_calibrate_vfifo_find_dqs_en_phase (alt_u32 grp)
{
	alt_u32 v;
	alt_u32 found;
	t_btfld bit_chk;

	found = 0;

	//USER first push vfifo until we get a passing read 
	for (v = 0; v < VFIFO_SIZE && found == 0;) {
		if (rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
			found = 1;
		}

		if (!found) {
			//USER fiddle with FIFO
			rw_mgr_incr_vfifo(grp, &v);
		}
	}

	return found;
}
#endif

#if DDRX
#if NEWVERSION_DQSEN

alt_u32 rw_mgr_mem_calibrate_vfifo_find_dqs_en_phase (alt_u32 grp)
{
	alt_u32 i, d, v, p;
	alt_u32 max_working_cnt;
	alt_u32 fail_cnt;
	t_btfld bit_chk;
	alt_u32 dtaps_per_ptap;
	alt_u32 found_begin, found_end;
	alt_u32 work_bgn, work_mid, work_end, tmp_delay;

	WRITE_SCC_DQS_EN_DELAY(grp, 0);
	WRITE_SCC_DQS_EN_PHASE(grp, 0);
	scc_mgr_load_dqs (grp);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	fail_cnt = 0;
	
	//USER **************************************************************
	//USER * Step 0 : Determine number of delay taps for each phase tap *
	
	dtaps_per_ptap = 0;
	tmp_delay = 0;
	while (tmp_delay < IO_DELAY_PER_OPA_TAP) {
		dtaps_per_ptap++;
		tmp_delay += IO_DELAY_PER_DCHAIN_TAP;
	}
	dtaps_per_ptap--;
	tmp_delay = 0;	

	//USER *********************************************************
	//USER * Step 1 : First push vfifo until we get a failing read *
	for (v = 0; v < VFIFO_SIZE; ) {
		if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
			fail_cnt++;

			if (fail_cnt == 2) {
				break;
			}
		}

		//USER fiddle with FIFO
		rw_mgr_incr_vfifo(grp, &v);
	}

	if (v >= VFIFO_SIZE) {
		//USER no failing read found!! Something must have gone wrong
		return 0;
	}

	max_working_cnt = 0;
	
	//USER ********************************************************
	//USER * step 2: find first working phase, increment in ptaps *
	found_begin = 0;
	work_bgn = 0;
	for (d = 0; d <= dtaps_per_ptap; d++, tmp_delay += IO_DELAY_PER_DCHAIN_TAP) {
		work_bgn = tmp_delay;
		WRITE_SCC_DQS_EN_DELAY(grp, d);
				
		for (i = 0; i < VFIFO_SIZE; i++) {
			for (p = 0; p <= IO_DQS_EN_PHASE_MAX; p++, work_bgn += IO_DELAY_PER_OPA_TAP) {
				WRITE_SCC_DQS_EN_PHASE(grp, p);
				scc_mgr_load_dqs (grp);
				IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

				if (rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
					max_working_cnt = 1;
					found_begin = 1;
					break;
				}
			}
			
			if (found_begin) {
				break;
			}
			
			if (p > IO_DQS_EN_PHASE_MAX) {
				//USER fiddle with FIFO
				rw_mgr_incr_vfifo(grp, &v);
			}
		}
		
		if (found_begin) {
			break;
		}
	}
	
	if (i >= VFIFO_SIZE) {
		//USER cannot find working solution 
		return 0;
	}
	
	work_end = work_bgn;

	//USER  If d is 0 then the working window covers a phase tap and we can follow the old procedure
	//USER 	otherwise, we've found the beginning, and we need to increment the dtaps until we find the end 
	if (d == 0) {
		//USER ********************************************************************
		//USER * step 3a: if we have room, back off by one and increment in dtaps *
			
		//USER Special case code for backing up a phase 
		if (p == 0) {
			p = IO_DQS_EN_PHASE_MAX ;
			rw_mgr_decr_vfifo(grp, &v);
		} else {
			p = p - 1;
		}
		tmp_delay = work_bgn - IO_DELAY_PER_OPA_TAP;
		WRITE_SCC_DQS_EN_PHASE(grp, p);
			
		found_begin = 0;
		for (d = 0; d <= IO_DQS_EN_DELAY_MAX && tmp_delay < work_bgn; d++, tmp_delay += IO_DELAY_PER_DCHAIN_TAP) {

			WRITE_SCC_DQS_EN_DELAY(grp, d);
			scc_mgr_load_dqs (grp);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
				
			if (rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
				found_begin = 1;
				work_bgn = tmp_delay;
				break;
			}
		}
			
		//USER We have found a working dtap before the ptap found above 
		if (found_begin == 1) {
			max_working_cnt++;
		} 
			
		//USER Restore VFIFO to old state before we decremented it 
		p = p + 1;
		if (p > IO_DQS_EN_PHASE_MAX) {
			p = 0;
			rw_mgr_incr_vfifo(grp, &v);
		}
			
		WRITE_SCC_DQS_EN_DELAY(grp, 0);
		
		//USER ***********************************************************************************
		//USER * step 4a: go forward from working phase to non working phase, increment in ptaps *
		p = p + 1;
		work_end += IO_DELAY_PER_OPA_TAP;
		if (p > IO_DQS_EN_PHASE_MAX) {
			//USER fiddle with FIFO
			p = 0;
			rw_mgr_incr_vfifo(grp, &v);
		}
		
		found_end = 0;
		for (; i < VFIFO_SIZE + 1; i++) {
			for (; p <= IO_DQS_EN_PHASE_MAX; p++, work_end += IO_DELAY_PER_OPA_TAP) {
				WRITE_SCC_DQS_EN_PHASE(grp, p);
				scc_mgr_load_dqs (grp);
				IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
				
				if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
					found_end = 1;
					break;
				} else {
					max_working_cnt++;
				}
			}
			
			if (found_end) {
				break;
			}
			
			if (p > IO_DQS_EN_PHASE_MAX) {
				//USER fiddle with FIFO
				rw_mgr_incr_vfifo(grp, &v);
				p = 0;
			}		
		}
		
		if (i >= VFIFO_SIZE + 1) {
			//USER cannot see edge of failing read 
			return 0;
		}
		
		//USER *********************************************************
		//USER * step 5a:  back off one from last, increment in dtaps  *
			
		//USER Special case code for backing up a phase 
		if (p == 0) {
			p = IO_DQS_EN_PHASE_MAX;
			rw_mgr_decr_vfifo(grp, &v);
		} else {
			p = p - 1;
		}
		
		work_end -= IO_DELAY_PER_OPA_TAP;
		WRITE_SCC_DQS_EN_PHASE(grp, p);
		
		//USER * The actual increment of dtaps is done outside of the if/else loop to share code
		d = 0;
	
	} else {

		//USER ********************************************************************
		//USER * step 3-5b:  Find the right edge of the window using delay taps   *		
		
		work_end = work_bgn;
		
		//USER * The actual increment of dtaps is done outside of the if/else loop to share code
		
		//USER Only here to counterbalance a subtract later on which is not needed if this branch
		//USER  of the algorithm is taken 
		max_working_cnt++;
	}

	//USER The dtap increment to find the failing edge is done here
		for (; d <= IO_DQS_EN_DELAY_MAX; d++, work_end += IO_DELAY_PER_DCHAIN_TAP) {

			WRITE_SCC_DQS_EN_DELAY(grp, d);
			scc_mgr_load_dqs (grp);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
				break;
			}
		}

	//USER Go back to working dtap 
	if (d != 0) {
		work_end -= IO_DELAY_PER_DCHAIN_TAP;
	} 
		
	
	if (work_end > work_bgn) {
		//USER we have a working range 
	} else {
		//USER nil range 
		return 0;
	}
	
	//USER ********************************************
	//USER * step 6:  Find the centre of the window   *
		
	work_mid = (work_bgn + work_end) / 2;
	tmp_delay = 0;
	
	//USER Get the middle delay to be less than a VFIFO delay 
	for (p = 0; p <= IO_DQS_EN_PHASE_MAX; p++, tmp_delay += IO_DELAY_PER_OPA_TAP);
	while(work_mid > tmp_delay) work_mid -= tmp_delay;
	tmp_delay = 0;
	for (p = 0; p <= IO_DQS_EN_PHASE_MAX && tmp_delay < work_mid; p++, tmp_delay += IO_DELAY_PER_OPA_TAP);
	tmp_delay -= IO_DELAY_PER_OPA_TAP;
	for (d = 0; d <= IO_DQS_EN_DELAY_MAX && tmp_delay < work_mid; d++, tmp_delay += IO_DELAY_PER_DCHAIN_TAP);
	
	gbl->export_dqse_window[grp] = max_working_cnt -1;
	WRITE_SCC_DQS_EN_PHASE(grp, p-1);
	WRITE_SCC_DQS_EN_DELAY(grp, d);
	scc_mgr_load_dqs (grp);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
	
	//USER push vfifo until we can successfully calibrate 

	for (i = 0; i < VFIFO_SIZE; i++) {
		if (rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
			break;
		}

		//USER fiddle with FIFO
		rw_mgr_incr_vfifo(grp, &v);
	}

	if (i >= VFIFO_SIZE) {
		return 0;
	}

	return 1;
}

#if 0

alt_u32 rw_mgr_mem_calibrate_vfifo_find_dqs_en_phase (alt_u32 grp)
{
	alt_u32 i, d, v, p;
	alt_u32 min_working_p, max_working_p, min_working_d, max_working_d, max_working_cnt;
	alt_u32 fail_cnt;
	t_btfld bit_chk;
	alt_u32 dtaps_per_ptap;
	alt_u32 found_begin, found_end;
	alt_u32 tmp_delay;

	WRITE_SCC_DQS_EN_DELAY(grp, 0);
	WRITE_SCC_DQS_EN_PHASE(grp, 0);
	scc_mgr_load_dqs (grp);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	fail_cnt = 0;
	
	//USER **************************************************************
	//USER * Step 0 : Determine number of delay taps for each phase tap *
	
	dtaps_per_ptap = 0;
	tmp_delay = 0;
	while (tmp_delay < IO_DELAY_PER_OPA_TAP) {
		dtaps_per_ptap++;
		tmp_delay += IO_DELAY_PER_DCHAIN_TAP;
	}
	dtaps_per_ptap--;

	//USER *********************************************************
	//USER * Step 1 : First push vfifo until we get a failing read *
	for (v = 0; v < VFIFO_SIZE; ) {
		if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
			fail_cnt++;

			if (fail_cnt == 2) {
				break;
			}
		}

		//USER fiddle with FIFO
		rw_mgr_incr_vfifo(grp, &v);
	}

	if (i >= VFIFO_SIZE) {
		//USER no failing read found!! Something must have gone wrong
		return 0;
	}

	max_working_cnt = 0;
	min_working_p = 0;
	
	//USER ********************************************************
	//USER * step 2: find first working phase, increment in ptaps *
	found_begin = 0;
	for (d = 0; d <= dtaps_per_ptap; d++) {
		WRITE_SCC_DQS_EN_DELAY(grp, d);
				
		for (i = 0; i < VFIFO_SIZE; i++) {
			for (p = 0; p <= IO_DQS_EN_PHASE_MAX; p++) {
				WRITE_SCC_DQS_EN_PHASE(grp, p);
				scc_mgr_load_dqs (grp);
				IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

				if (rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
					max_working_cnt = 1;
					found_begin = 1;
					break;
				}
			}
			
			if (found_begin) {
				break;
			}
			
			if (p > IO_DQS_EN_PHASE_MAX) {
				//USER fiddle with FIFO
				rw_mgr_incr_vfifo(grp, &v);
			}
		}
		
		if (found_begin) {
			break;
		}
	}
	
	if (i >= VFIFO_SIZE) {
		//USER cannot find working solution 
		return 0;
	}
		
	min_working_p = p;

	//USER  If d is 0 then the working window covers a phase tap and we can follow the old procedure
	//USER 	otherwise, we've found the beginning, and we need to increment the dtaps until we find the end 
	if (d == 0) {
		//USER ********************************************************************
		//USER * step 3a: if we have room, back off by one and increment in dtaps *
		min_working_d = 0;

		//USER Special case code for backing up a phase 
		if (p == 0) {
			p = IO_DQS_EN_PHASE_MAX ;
			rw_mgr_decr_vfifo(grp, &v);
		} else {
			p = p - 1;
		}
		WRITE_SCC_DQS_EN_PHASE(grp, p);
		
		found_begin = 0;
		for (d = 0; d <= dtaps_per_ptap; d++) {

			WRITE_SCC_DQS_EN_DELAY(grp, d);
			scc_mgr_load_dqs (grp);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
			
			if (rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
				found_begin = 1;
				min_working_d = d;
				break;
			}
		}
		
		//USER We have found a working dtap before the ptap found above 
		if (found_begin == 1) {
			min_working_p = p;
			max_working_cnt++;
		} 
		
		//USER Restore VFIFO to old state before we decremented it 
		p = p + 1;
		if (p > IO_DQS_EN_PHASE_MAX) {
			p = 0;
			rw_mgr_incr_vfifo(grp, &v);
		}
		
		WRITE_SCC_DQS_EN_DELAY(grp, 0);

		
		//USER ***********************************************************************************
		//USER * step 4a: go forward from working phase to non working phase, increment in ptaps *
		p = p + 1;
		if (p > IO_DQS_EN_PHASE_MAX) {
			//USER fiddle with FIFO
			p = 0;
			rw_mgr_incr_vfifo(grp, &v);
		}
		
		found_end = 0;
		for (; i < VFIFO_SIZE+1; i++) {
			for (; p <= IO_DQS_EN_PHASE_MAX; p++) {
				WRITE_SCC_DQS_EN_PHASE(grp, p);
				scc_mgr_load_dqs (grp);
				IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
				
				if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
					found_end = 1;
					break;
				} else {
					max_working_cnt++;
				}
			}
			
			if (found_end) {
				break;
			}
			
			if (p > IO_DQS_EN_PHASE_MAX) {
				//USER fiddle with FIFO
				rw_mgr_incr_vfifo(grp, &v);
				p = 0;
			}		
		}
		
		if (i >= VFIFO_SIZE+1) {
			//USER cannot see edge of failing read 
			return 0;
		}
		
		//USER *********************************************************
		//USER * step 5a:  back off one from last, increment in dtaps  *
		max_working_d = 0;
			
		//USER Special case code for backing up a phase 
		if (p == 0) {
			p = IO_DQS_EN_PHASE_MAX;
			rw_mgr_decr_vfifo(grp, &v);
		} else {
			p = p - 1;
		}
		
		max_working_p = p;
		WRITE_SCC_DQS_EN_PHASE(grp, p);
		
		for (d = 0; d <= IO_DQS_EN_DELAY_MAX; d++) {

			WRITE_SCC_DQS_EN_DELAY(grp, d);
			scc_mgr_load_dqs (grp);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
			
			if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
				break;
			}
		}
		
		//USER Go back to working dtap 
		if (d != 0) {
			max_working_d = d - 1;
		} 
	
	} else {

		//USER ********************************************************************
		//USER * step 3-5b:  Find the right edge of the window using delay taps   *		
		
		max_working_p = min_working_p;
		min_working_d = d;
		
		for (; d <= IO_DQS_EN_DELAY_MAX; d++) {
			WRITE_SCC_DQS_EN_DELAY(grp, d);
			scc_mgr_load_dqs (grp);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
				break;
			}
		}

		//USER Go back to working dtap 
		if (d != 0) {
			max_working_d = d - 1;
		} 
		
		//USER Only here to counterbalance a subtract later on which is not needed if this branch
		//USER of the algorithm is taken 
		max_working_cnt++;		
	}

	//USER ********************************************
	//USER * step 6:  Find the centre of the window   *

	//USER If the number of working phases is even we will step back a phase and find the
	//USER 	edge with a larger delay chain tap 
	if ((max_working_cnt & 1) == 0) {
		p = min_working_p + (max_working_cnt-1)/2;
		
		//USER Special case code for backing up a phase 
		if (max_working_p == 0) {
			max_working_p = IO_DQS_EN_PHASE_MAX;
			rw_mgr_decr_vfifo(grp, &v);
		} else {
			max_working_p = max_working_p - 1;
		}
		
		WRITE_SCC_DQS_EN_PHASE(grp, max_working_p);
		
		//USER Code to determine at which dtap we should start searching again for a failure
		//USER If we've moved back such that the max and min p are the same, we should start searching
		//USER from where the window actually exists
		if (max_working_p == min_working_p) {
			d = min_working_d;
		} else {
			d = max_working_d;
		}
		
		for (; d <= IO_DQS_EN_DELAY_MAX; d++) {
			WRITE_SCC_DQS_EN_DELAY(grp, d);
			scc_mgr_load_dqs (grp);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
				break;
			}
		}

		//USER Go back to working dtap 
		if (d != 0) {
			max_working_d = d - 1;
		}
	} else {
		p = min_working_p + (max_working_cnt)/2;
	}
	
	while (p > IO_DQS_EN_PHASE_MAX) {
		p -= (IO_DQS_EN_PHASE_MAX + 1);
	}	
		
	d = (min_working_d + max_working_d)/2;
	
	gbl->export_dqse_window[grp] = max_working_cnt - 1;
	WRITE_SCC_DQS_EN_PHASE(grp, p);
	WRITE_SCC_DQS_EN_DELAY(grp, d);
	
	scc_mgr_load_dqs (grp);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
	
	//USER push vfifo until we can successfully calibrate 

	for (i = 0; i < VFIFO_SIZE; i++) {
		if (rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
			break;
		}

		//USER fiddle with FIFO
		rw_mgr_incr_vfifo(grp, &v);
	}

	if (i >= VFIFO_SIZE) {
		return 0;
	}

	return 1;
}

#endif

#else

alt_u32 rw_mgr_mem_calibrate_vfifo_find_dqs_en_phase (alt_u32 grp)
{
	alt_u32 i, j, v, d;
	alt_u32 min_working_d, max_working_cnt;
	alt_u32 fail_cnt;
	t_btfld bit_chk;
	alt_u32 delay_per_ptap_mid;

	WRITE_SCC_DQS_EN_DELAY(grp, 0);
	WRITE_SCC_DQS_EN_PHASE(grp, 0);
	scc_mgr_load_dqs (grp);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	fail_cnt = 0;

	//USER first push vfifo until we get a failing read 

	for (v = 0; v < VFIFO_SIZE; v++) {
		if (!rw_mgr_mem_calibrate_read_test (grp, 1, PASS_ONE_BIT, &bit_chk, 0)) {
			fail_cnt++;

			if (fail_cnt == 2) {
				break;
			}
		}

		//USER fiddle with FIFO

		if (!HALF_RATE_MODE || (v & 1) == 1) {
			IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, grp);
		} else {
			IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, grp);
		}
	}

	if (v >= VFIFO_SIZE) {
		//USER no failing read found!! Something must have gone wrong

		return 0;
	}

	max_working_cnt = 0;
	min_working_d = 0;

	for (i = 0; i < VFIFO_SIZE+1; i++) {
		for (d = 0; d <= IO_DQS_EN_PHASE_MAX; d++) {
			WRITE_SCC_DQS_EN_PHASE(grp, d);
			scc_mgr_load_dqs (grp);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			rw_mgr_mem_calibrate_read_test (grp, NUM_READ_PB_TESTS, PASS_ONE_BIT, &bit_chk, 0);
			if (bit_chk) {
				//USER passing read 

				if (max_working_cnt == 0) {
					min_working_d = d;
				}

				max_working_cnt++;
			} else {
				if (max_working_cnt > 0) {
					//USER already have one working value 
					break;
				}
			}
		}

		if (d > IO_DQS_EN_PHASE_MAX) {
			//USER fiddle with FIFO

			if (!HALF_RATE_MODE || (v & 1) == 1) {
				IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, grp);
			} else {
				IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, grp);
			}

			v++;
		} else {
			//USER found working solution! 

			d = min_working_d + (max_working_cnt - 1) / 2;

			while (d > IO_DQS_EN_PHASE_MAX) {
				d -= (IO_DQS_EN_PHASE_MAX + 1);
			}

			//USER export discovered window size 

			gbl->export_dqse_window[grp] = max_working_cnt;

			break;
		}
	}

	if (i >= VFIFO_SIZE+1) {
		//USER cannot find working solution or cannot see edge of failing read 

		return 0;
	}

	//USER in the case the number of working steps is even, use 50ps taps to further center the window 

	if ((max_working_cnt & 1) == 0) {
		delay_per_ptap_mid = IO_DELAY_PER_OPA_TAP / 2;

		//USER increment in 50ps taps until we reach the required amount 

		for (i = 0, j = 0; i <= IO_DQS_EN_DELAY_MAX && j < delay_per_ptap_mid; i++, j += IO_DELAY_PER_DCHAIN_TAP);

		WRITE_SCC_DQS_EN_DELAY(grp, i - 1);
	}

	WRITE_SCC_DQS_EN_PHASE(grp, d);
	scc_mgr_load_dqs (grp);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	//USER push vfifo until we can successfully calibrate 

	for (i = 0; i < VFIFO_SIZE; i++) {
		if (rw_mgr_mem_calibrate_read_test (grp, NUM_READ_PB_TESTS, PASS_ONE_BIT, &bit_chk, 0)) {
			break;
		}

		//USER fiddle with FIFO

		if (!HALF_RATE_MODE || (v & 1) == 1) {
			IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, grp);
		} else {
			IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, grp);
		}

		v++;
	}

	if (i >= VFIFO_SIZE) {
		return 0;
	}

	return 1;
}

#endif
#endif

//USER per-bit deskew DQ and center 

#if NEWVERSION_RDDESKEW

alt_u32 rw_mgr_mem_calibrate_vfifo_center (alt_u32 grp, alt_u32 test_bgn)
{
	alt_u32 i, p, d, min_index;
	//USER Store these as signed since there are comparisons with signed numbers
	t_btfld bit_chk;
	t_btfld sticky_bit_chk;
	alt_32 left_edge[RW_MGR_MEM_DQ_PER_READ_DQS];
	alt_32 right_edge[RW_MGR_MEM_DQ_PER_READ_DQS];
	alt_32 mid;
	alt_32 orig_mid_min, mid_min;
	alt_32 new_dqs, start_dqs, start_dqs_en, shift_dq;
	alt_u32 dq_margin, dqs_margin;
	alt_u32 stop;

	start_dqs = READ_SCC_DQS_IN_DELAY(grp);
	if (DDRX) {
		start_dqs_en = READ_SCC_DQS_EN_DELAY(grp);
	}
	
	//USER per-bit deskew 
		
	//USER set the left and right edge of each bit to an illegal value 
	//USER use (IO_IO_IN_DELAY_MAX + 1) as an illegal value 
	sticky_bit_chk = 0;
	for (i = 0; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {
		left_edge[i]  = IO_IO_IN_DELAY_MAX + 1;
		right_edge[i] = IO_IO_IN_DELAY_MAX + 1;
	}
	
	//USER Search for the left edge of the window for each bit
	for (d = 0; d <= IO_IO_IN_DELAY_MAX; d++) {
		scc_mgr_apply_group_dq_in_delay (test_bgn, d);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Stop searching when the read test doesn't pass AND when we've seen a passing read on every bit
		stop = !rw_mgr_mem_calibrate_read_test (grp, NUM_READ_PB_TESTS, PASS_ONE_BIT, &bit_chk, 0);
		sticky_bit_chk = sticky_bit_chk | bit_chk;
		stop = stop && (sticky_bit_chk == param->read_correct_mask);
		
		if (stop == 1) {
			break;
		} else {
			for (i = 0; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {
				if (bit_chk & 1) {
					//USER Remember a passing test as the left_edge
					left_edge[i] = d;
				} else {
					//USER If a left edge has not been seen yet, then a future passing test will mark this edge as the right edge 
					if (left_edge[i] == IO_IO_IN_DELAY_MAX + 1) {
						right_edge[i] = -(d + 1);
					}
				}
				bit_chk = bit_chk >> 1;
			}
		}
	}

	//USER Reset DQ delay chains to 0 
	scc_mgr_apply_group_dq_in_delay (test_bgn, 0);
	sticky_bit_chk = 0;
	for (i = 0; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {

		//USER Check for cases where we haven't found the left edge, which makes our assignment of the the 
		//USER right edge invalid.  Reset it to the illegal value. 
		if ((left_edge[i] == IO_IO_IN_DELAY_MAX + 1) && (right_edge[i] != IO_IO_IN_DELAY_MAX + 1)) {
			right_edge[i] = IO_IO_IN_DELAY_MAX + 1;
		}
		
		//USER Reset sticky bit (except for bits where we have seen both the left and right edge) 
		sticky_bit_chk = sticky_bit_chk << 1;
		if ((left_edge[i] != IO_IO_IN_DELAY_MAX + 1) && (right_edge[i] != IO_IO_IN_DELAY_MAX + 1)) {
			sticky_bit_chk = sticky_bit_chk | 1;
		}
	}
	
	//USER Search for the right edge of the window for each bit 
	for (d = 0; d <= IO_DQS_IN_DELAY_MAX - start_dqs; d++) {
		WRITE_SCC_DQS_IN_DELAY(grp, d + start_dqs);
		if (DDRX) {
			alt_u32 delay = d + start_dqs_en;
			if (delay > IO_DQS_EN_DELAY_MAX) {
				delay = IO_DQS_EN_DELAY_MAX;
			}
			WRITE_SCC_DQS_EN_DELAY(grp, delay);
		}
		scc_mgr_load_dqs (grp);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Stop searching when the read test doesn't pass AND when we've seen a passing read on every bit 
		stop = !rw_mgr_mem_calibrate_read_test (grp, NUM_READ_PB_TESTS, PASS_ONE_BIT, &bit_chk, 0);
		sticky_bit_chk = sticky_bit_chk | bit_chk;
		stop = stop && (sticky_bit_chk == param->read_correct_mask);
		
		if (stop == 1) {
			break;
		} else {
			for (i = 0; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {
				if (bit_chk & 1) {
					//USER Remember a passing test as the right_edge 
					right_edge[i] = d;
				} else {
					if (d != 0) {
						//USER If a right edge has not been seen yet, then a future passing test will mark this edge as the left edge 
						if (right_edge[i] == IO_IO_IN_DELAY_MAX + 1) {
							left_edge[i] = -(d + 1);
						}
					} else {
						//USER d = 0 failed, but it passed when testing the left edge, so it must be marginal, set it to -1
						if (right_edge[i] == IO_IO_IN_DELAY_MAX + 1) {
							right_edge[i] = -1;
						}
					}	
				}
				
				bit_chk = bit_chk >> 1;
			}
		}
	}
	
	//USER Check that all bits have a window
	for (i = 0; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {
		if ((left_edge[i] == IO_IO_IN_DELAY_MAX + 1) || (right_edge[i] == IO_IO_IN_DELAY_MAX + 1)) {
			return 0;
		}
	}
	
	//USER Find middle of window for each DQ bit 
	mid_min = left_edge[0] - right_edge[0];
	min_index = 0;
	for (i = 1; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {
		mid = left_edge[i] - right_edge[i];
		if (mid < mid_min) {
			mid_min = mid;
			min_index = i;
		}
	}
	//USER  -mid_min/2 represents the amount that we need to move DQS.  If mid_min is odd we'll need to add one to make sure 
	//USER 	the rounding in further calculations is correct 
	mid_min = (mid_min+1)/2;
	
	//USER Determine the amount we can change DQS (which is -mid_min)
	orig_mid_min = mid_min;
#if ENABLE_DQS_IN_CENTERING
	new_dqs = start_dqs - mid_min;
	if (new_dqs > IO_DQS_IN_DELAY_MAX) {
		new_dqs = IO_DQS_IN_DELAY_MAX;
	} else if (new_dqs < 0) {
		new_dqs = 0;
	} 
	mid_min = start_dqs - new_dqs;
	
	if (DDRX) {
		if (start_dqs_en - mid_min > IO_DQS_EN_DELAY_MAX) {
			mid_min += start_dqs_en - mid_min - IO_DQS_EN_DELAY_MAX;
		} else if (start_dqs_en - mid_min < 0) {
			mid_min += start_dqs_en - mid_min;
		}
	}
	new_dqs = start_dqs - mid_min;
#else
	new_dqs = start_dqs;
	mid_min = 0;
#endif
	
	//USER Initialize data for export structures 
	dqs_margin = IO_IO_IN_DELAY_MAX + 1;
	dq_margin  = IO_IO_IN_DELAY_MAX + 1;
	
	//USER add delay to bring centre of all DQ windows to the same "level" 
	for (i = 0, p = test_bgn; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++, p++) {
		//USER Use values before divide by 2 to reduce round off error 
		shift_dq = (left_edge[i] - right_edge[i] - (left_edge[min_index] - right_edge[min_index]))/2  + (orig_mid_min - mid_min);
		
		if (shift_dq + (alt_32)READ_SCC_DQ_IN_DELAY(i) > (alt_32)IO_IO_IN_DELAY_MAX) {
			shift_dq = (alt_32)IO_IO_IN_DELAY_MAX - READ_SCC_DQ_IN_DELAY(i);
		} else if (shift_dq + (alt_32)READ_SCC_DQ_IN_DELAY(i) < 0) {
			shift_dq = -(alt_32)READ_SCC_DQ_IN_DELAY(i);
		} 
		WRITE_SCC_DQ_IN_DELAY(i, READ_SCC_DQ_IN_DELAY(i) + shift_dq);
		scc_mgr_load_dq (p, p);
		
		//USER To determine values for export structures 
		if (left_edge[i] - shift_dq + (-mid_min) < dq_margin) {
			dq_margin = left_edge[i] - shift_dq + (-mid_min);
		}
		if (right_edge[i] + shift_dq - (-mid_min) < dqs_margin) {
			dqs_margin = right_edge[i] + shift_dq - (-mid_min);
		}
	}

#if ENABLE_DQS_IN_CENTERING	
	//USER Move DQS 
	WRITE_SCC_DQS_IN_DELAY(grp, new_dqs);
	if (DDRX) {
		WRITE_SCC_DQS_EN_DELAY(grp, start_dqs_en - mid_min);
	}
#else
	WRITE_SCC_DQS_IN_DELAY(grp, start_dqs);
	if (DDRX) {
		WRITE_SCC_DQS_EN_DELAY(grp, start_dqs_en);
	}	
#endif	
	scc_mgr_load_dqs (grp);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	//USER Export values 
	gbl->fom_in += dq_margin + dqs_margin;
	gbl->export_dqsi_margin[grp] = dqs_margin;
	gbl->export_dqi_margin[grp] = dq_margin;

	return (dq_margin + dqs_margin) > 0;
}

#else

alt_u32 rw_mgr_mem_calibrate_vfifo_center (alt_u32 grp, alt_u32 test_bgn)
{
	alt_u32 i, p, d;
	alt_u32 mid;
	t_btfld bit_chk;
	alt_u32 max_working_dq[RW_MGR_MEM_DQ_PER_READ_DQS];
	alt_u32 dq_margin, dqs_margin;
	alt_u32 start_dqs;

	//USER per-bit deskew.
	//USER start of the per-bit sweep with the minimum working delay setting for
	//USER all bits.

	for (i = 0; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {
		max_working_dq[i] = 0;
	}

	for (d = 1; d <= IO_IO_IN_DELAY_MAX; d++) {
		scc_mgr_apply_group_dq_in_delay (test_bgn, d);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_read_test (grp, NUM_READ_PB_TESTS, PASS_ONE_BIT, &bit_chk, 0)) {
			break;
		} else {
			for (i = 0; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {
				if (bit_chk & 1) {
					max_working_dq[i] = d;
				}
				bit_chk = bit_chk >> 1;
			}
		}
	}

	//USER determine minimum working value for DQ 

	dq_margin = IO_IO_IN_DELAY_MAX;

	for (i = 0; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++) {
		if (max_working_dq[i] < dq_margin) {
			dq_margin = max_working_dq[i];
		}
	}

	//USER add delay to bring all DQ windows to the same "level" 

	for (i = 0, p = test_bgn; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++, p++) {
		if (max_working_dq[i] > dq_margin) {
			WRITE_SCC_DQ_IN_DELAY(i, max_working_dq[i] - dq_margin);
		} else {
			WRITE_SCC_DQ_IN_DELAY(i, 0);
		}

		scc_mgr_load_dq (p, p);
	}

	//USER sweep DQS window, may potentially have more window due to per-bit-deskew that was done
	//USER in the previous step.

	start_dqs = READ_SCC_DQS_IN_DELAY(grp);

	for (d = start_dqs + 1; d <= IO_DQS_IN_DELAY_MAX; d++) {
		WRITE_SCC_DQS_IN_DELAY(grp, d);
		scc_mgr_load_dqs (grp);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_read_test (grp, NUM_READ_TESTS, PASS_ALL_BITS, &bit_chk, 0)) {
			break;
		}
	}

	WRITE_SCC_DQS_IN_DELAY(grp, start_dqs);

	//USER margin on the DQS pin 

	dqs_margin = d - start_dqs - 1;

	//USER find mid point, +1

	mid = (dq_margin + dqs_margin + 1) / 2;

	gbl->fom_in += dq_margin + dqs_margin;

#if ENABLE_DQS_IN_CENTERING
	//USER center DQS ... if the headroom is setup properly we shouldn't need to 

	if (dqs_margin > mid) {
		WRITE_SCC_DQS_IN_DELAY(grp, READ_SCC_DQS_IN_DELAY(grp) + dqs_margin - mid);

		if (DDRX) {
			alt_u32 delay = READ_SCC_DQS_EN_DELAY(grp) + dqs_margin - mid;

			if (delay > IO_DQS_EN_DELAY_MAX) {
				delay = IO_DQS_EN_DELAY_MAX;
			}

			WRITE_SCC_DQS_EN_DELAY(grp, delay);
		}
	}
#endif

	scc_mgr_load_dqs (grp);

	//USER center DQ 

	if (dq_margin > mid) {
		for (i = 0, p = test_bgn; i < RW_MGR_MEM_DQ_PER_READ_DQS; i++, p++) {
			WRITE_SCC_DQ_IN_DELAY(i, READ_SCC_DQ_IN_DELAY(i) + dq_margin - mid);
			scc_mgr_load_dq (p, p);
		}

		dqs_margin += dq_margin - mid;
		dq_margin  -= dq_margin - mid;
	}

	//USER save in export structures 

	gbl->export_dqsi_margin[grp] = dqs_margin;
	gbl->export_dqi_margin[grp] = dq_margin;

	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	return (dq_margin + dqs_margin) > 0;
}

#endif

//USER calibrate the read valid prediction FIFO.
//USER 
//USER  - read valid prediction will consist of finding a good DQS enable phase, DQS enable delay, DQS input phase, and DQS input delay.
//USER  - we also do a per-bit deskew on the DQ lines.

#if DYNAMIC_CALIBRATION_MODE || STATIC_QUICK_CALIBRATION

#if !ENABLE_SUPER_QUICK_CALIBRATION

//USER VFIFO Calibration -- Quick Calibration
alt_u32 rw_mgr_mem_calibrate_vfifo (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 v, d;
	alt_u32 found;
	t_btfld bit_chk;

	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_VFIFO);

	//USER Load up the patterns used by read calibration 

	rw_mgr_mem_calibrate_read_load_patterns ();

	//USER maximum phase values for the sweep 


	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	found = 0;

	for (v = 0; v < VFIFO_SIZE && found == 0; v++) {
		for (d = 0; d <= IO_DQS_EN_PHASE_MAX && found == 0; d++) {
			if (DDRX)
			{
				WRITE_SCC_DQS_EN_PHASE(g, d);
				scc_mgr_load_dqs (g);
				IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
			}
			
			//USER calibrate the vfifo with the current dqs enable phase setting 

			if (rw_mgr_mem_calibrate_read_test (g, 1, PASS_ONE_BIT, &bit_chk, 0)) {
				found = 1;
			}
		}

		if (!found) {
			if (!HALF_RATE_MODE || (v & 1) == 1) {
				IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, g);
			} else {
				IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, g);
			}
		}
	}

	return found;
}

#else

//USER VFIFO Calibration -- Super Quick Calibration
alt_u32 rw_mgr_mem_calibrate_vfifo (alt_u32 grp, alt_u32 test_bgn2)
{
	alt_u32 g, v, d;
	alt_u32 test_bgn;
	alt_u32 found;
	t_btfld bit_chk;
	alt_u32 phase_increment;
	alt_u32 final_v_setting = 0;
	alt_u32 final_d_setting = 0;
	
	//USER The first call to this function will calibrate all groups
	if (grp !=0) {
		return 1;
	}

	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_VFIFO);

	//USER Load up the patterns used by read calibration 

	rw_mgr_mem_calibrate_read_load_patterns ();

	//USER maximum phase values for the sweep 

	//USER Calibrate group 0
	g = 0;
	test_bgn = 0;

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	found = 0;

	//USER In behavioral simulation only phases 0 and IO_DQS_EN_PHASE_MAX/2 are relevant
	//USER All other values produces the same results as those 2, so there's really no
	//USER point in sweeping them all
	phase_increment = (IO_DQS_EN_PHASE_MAX + 1) / 2;
	//USER Make sure phase_increment is > 0 to prevent infinite loop
	if (phase_increment == 0) phase_increment++;

	for (v = 0; v < VFIFO_SIZE && found == 0; v++) {
		for (d = 0; d <= IO_DQS_EN_PHASE_MAX && found == 0; d += phase_increment) {

			WRITE_SCC_DQS_EN_PHASE(g, d);
			scc_mgr_load_dqs (g);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			//USER calibrate the vfifo with the current dqs enable phase setting 

			if (rw_mgr_mem_calibrate_read_test (g, 1, PASS_ONE_BIT, &bit_chk, 0)) {
				found = 1;
				final_v_setting = v;
				final_d_setting = d;
			}
		}

		if (!found) {
			if (!HALF_RATE_MODE || (v & 1) == 1) {
				IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, g);
			} else {
				IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, g);
			}
		}
	}

	if (!found) return 0;

	//USER Now copy the calibration settings to all other groups
	for (g = 1, test_bgn = RW_MGR_MEM_DQ_PER_READ_DQS; (g < RW_MGR_MEM_IF_READ_DQS_WIDTH) && found; g++, test_bgn += RW_MGR_MEM_DQ_PER_READ_DQS) {
		//USER Set the VFIFO
		for (v = 0; v < final_v_setting; v++) {
			if (!HALF_RATE_MODE || (v & 1) == 1) {
				IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, g);
			} else {
				IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, g);
			}
		}

		//USER Set the proper phase
		WRITE_SCC_DQS_EN_PHASE(g, final_d_setting);
		scc_mgr_load_dqs (g);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Verify that things worked as expected
		if(!rw_mgr_mem_calibrate_read_test (g, 1, PASS_ONE_BIT, &bit_chk, 0)) {
			//USER Fail
			found = 0;
		}
	}

	return found;
}

#endif
#endif

#if DYNAMIC_CALIBRATION_MODE || STATIC_FULL_CALIBRATION

#if NEWVERSION_GW

//USER VFIFO Calibration -- Full Calibration
alt_u32 rw_mgr_mem_calibrate_vfifo (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 p, d;
	alt_u32 max_dq_out_delay;
	alt_u32 grp_calibrated;
	alt_u32 write_group, write_test_bgn;

	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_VFIFO);

	if (QDRII) {
		max_dq_out_delay = 0;
	} else {
		max_dq_out_delay = IO_IO_OUT1_DELAY_MAX;
	}

	if (DDRX) {
		write_group = g;
		write_test_bgn = test_bgn;
	} else {
		write_group = g / (RW_MGR_MEM_IF_READ_DQS_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH);
		write_test_bgn = g * RW_MGR_MEM_DQ_PER_WRITE_DQS;
	}


	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	grp_calibrated = 0;

	for (d = 0; d <= max_dq_out_delay && grp_calibrated == 0; d++) {

		if (DDRX || RLDRAMII) {
			scc_mgr_apply_group_all_out_delay (write_group, write_test_bgn, d);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
		}

		for (p = 0; p <= IO_DQDQS_OUT_PHASE_MAX && grp_calibrated == 0; p++) {
			//USER set a particular dqdqs phase 
			if (DDRX) {
				WRITE_SCC_DQDQS_OUT_PHASE(g, p);
				scc_mgr_load_dqs (g);
				IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
			}

			//USER Load up the patterns used by read calibration using current DQDQS phase 

			rw_mgr_mem_calibrate_read_load_patterns ();

			if (rw_mgr_mem_calibrate_vfifo_find_dqs_en_phase (g) &&
				rw_mgr_mem_calibrate_vfifo_center (g, test_bgn)) {
				grp_calibrated = 1;
			}
		}
	}

	if (grp_calibrated == 0) {
		gbl->error_stage = CAL_STAGE_VFIFO;
		gbl->error_group = g;
		return 0;
	}

	//USER Reset the delay chains back to zero if they have moved > 1 (check for > 1 because loop will increase d even when pass in first case)
	if (d > 1) {
		scc_mgr_apply_group_all_out_delay (write_group, write_test_bgn, 0);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
	}


	return 1;
}

#else

//USER VFIFO Calibration -- Full Calibration
alt_u32 rw_mgr_mem_calibrate_vfifo (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 p;
	alt_u32 grp_calibrated;

	//USER update info for sims 
	
	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_VFIFO);

	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	grp_calibrated = 0;

	for (p = 0; p <= IO_DQDQS_OUT_PHASE_MAX && grp_calibrated == 0; p++) {
		//USER set a particular dqdqs phase 
		if (DDRX) {
			WRITE_SCC_DQDQS_OUT_PHASE(g, p);
			scc_mgr_load_dqs (g);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
		}

		//USER Load up the patterns used by read calibration using current DQDQS phase 

		rw_mgr_mem_calibrate_read_load_patterns ();

		if (rw_mgr_mem_calibrate_vfifo_find_dqs_en_phase (g) &&
			rw_mgr_mem_calibrate_vfifo_center (g, test_bgn)) {
			grp_calibrated = 1;
		}
	}

	if (grp_calibrated == 0) {
		gbl->error_stage = CAL_STAGE_VFIFO;
		gbl->error_group = g;
		return 0;
	}


	return 1;
}

#endif

#endif

//USER Calibrate LFIFO to find smallest read latency

alt_u32 rw_mgr_mem_calibrate_lfifo (void)
{
	alt_u32 found_one;

	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_LFIFO);

	//USER Load up the patterns used by read calibration 

	rw_mgr_mem_calibrate_read_load_patterns ();

	found_one = 0;

	do {
		//USER reduce read latency and see if things are working
		//USER correctly

		gbl->curr_read_lat--;
		IOWR_32DIRECT (PHY_MGR_PHY_RLAT, 0, gbl->curr_read_lat);

		if (!rw_mgr_mem_calibrate_read_test_all_groups ()) {
			break;
		}

		found_one = 1;
	} while (gbl->curr_read_lat > 0);

	//USER reset the fifos to get pointers to known state 

	IOWR_32DIRECT (PHY_MGR_CMD_FIFO_RESET, 0, 0);

	if (found_one) {
		//USER add a fudge factor to the read latency that was determined 

		gbl->curr_read_lat += 2;
		IOWR_32DIRECT (PHY_MGR_PHY_RLAT, 0, gbl->curr_read_lat);

		return 1;
	} else {
		gbl->error_stage = CAL_STAGE_LFIFO;
		gbl->error_group = 0xff;

		return 0;
	}
}

//USER issue write test command.
//USER two variants are provided. one that just tests a write pattern and another that
//USER tests datamask functionality.

#if QDRII
void rw_mgr_mem_calibrate_write_test_issue (alt_u32 group, alt_u32 test_dm)
{
	alt_u32 quick_write_mode = ((STATIC_CALIB_STEPS) & CALIB_SKIP_WRITES) && ENABLE_SUPER_QUICK_CALIBRATION;
	if (test_dm) {
		IOWR_32DIRECT (RW_MGR_RESET_READ_DATAPATH, 0, 0);
		if(quick_write_mode) {
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x08);
		} else {
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x40);
		}
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_LFSR_WR_RD_DM_BANK_0);
		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, (group) << 2, __RW_MGR_LFSR_WR_RD_DM_BANK_0);
	} else {
		IOWR_32DIRECT (RW_MGR_RESET_READ_DATAPATH, 0, 0);
		if(quick_write_mode) {
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x08);
		} else {
			IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x40);
		}
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_LFSR_WR_RD_BANK_0);
		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, (group) << 2, __RW_MGR_LFSR_WR_RD_BANK_0);
	}
}
#else
void rw_mgr_mem_calibrate_write_test_issue (alt_u32 group, alt_u32 test_dm)
{
	alt_u32 mcc_instruction;
	alt_u32 quick_write_mode = ((STATIC_CALIB_STEPS) & CALIB_SKIP_WRITES) && ENABLE_SUPER_QUICK_CALIBRATION;

	//USER Set counter and jump addresses for the right
	//USER number of NOP cycles.
	//USER The number of supported NOP cycles can range from -1 to infinity
	//USER Three different cases are handled:
	//USER
	//USER 1. For a number of NOP cycles greater than 0, the RW Mgr looping
	//USER    mechanism will be used to insert the right number of NOPs
	//USER
	//USER 2. For a number of NOP cycles equals to 0, the micro-instruction
	//USER    issuing the write command will jump straight to the micro-instruction
	//USER    that turns on DQS (for DDRx), or outputs write data (for RLD), skipping
	//USER    the NOP micro-instruction all together
	//USER
	//USER 3. A number of NOP cycles equal to -1 indicates that DQS must be turned
	//USER    on in the same micro-instruction that issues the write command. Then we need
	//USER    to directly jump to the micro-instruction that sends out the data
	//USER
	//USER NOTE: Implementing this mechanism uses 2 RW Mgr jump-counters (2 and 3). One
	//USER       jump-counter (0) is used to perform multiple write-read operations.
	//USER       one counter left to issue this command in "multiple-group" mode.

	if(gbl->rw_wl_nop_cycles == -1)
	{
		#if DDRX
		//USER CNTR 2 - We want to execute the special write operation that
		//USER turns on DQS right away and then skip directly to the instruction that
		//USER sends out the data. We set the counter to a large number so that the
		//USER jump is always taken
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_2, 0, 0xFF);

		//USER CNTR 3 - Not used
		if(test_dm)
		{
			mcc_instruction = __RW_MGR_LFSR_WR_RD_DM_BANK_0_WL_1;
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_LFSR_WR_RD_DM_BANK_0_DATA);
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_3, 0, __RW_MGR_LFSR_WR_RD_DM_BANK_0_NOP);
		}
		else
		{
			mcc_instruction = __RW_MGR_LFSR_WR_RD_BANK_0_WL_1;
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_LFSR_WR_RD_BANK_0_DATA);
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_3, 0, __RW_MGR_LFSR_WR_RD_BANK_0_NOP);
		}
		
		#endif
	} 
	else if(gbl->rw_wl_nop_cycles == 0)
	{
		#if DDRX
		//USER CNTR 2 - We want to skip the NOP operation and go straight to
		//USER the DQS enable instruction. We set the counter to a large number so that the
		//USER jump is always taken
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_2, 0, 0xFF);

		//USER CNTR 3 - Not used
		if(test_dm)
		{
			mcc_instruction = __RW_MGR_LFSR_WR_RD_DM_BANK_0;
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_LFSR_WR_RD_DM_BANK_0_DQS);
		}
		else
		{
			mcc_instruction = __RW_MGR_LFSR_WR_RD_BANK_0;
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_LFSR_WR_RD_BANK_0_DQS);
		}
		#endif
		
		#if RLDRAMII
		//USER CNTR 2 - We want to skip the NOP operation and go straight to
		//USER the write data instruction. We set the counter to a large number so that the
		//USER jump is always taken
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_2, 0, 0xFF);

		//USER CNTR 3 - Not used
		if(test_dm)
		{
			mcc_instruction = __RW_MGR_LFSR_WR_RD_DM_BANK_0;
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_LFSR_WR_RD_DM_BANK_0_DATA);
		}
		else
		{
			mcc_instruction = __RW_MGR_LFSR_WR_RD_BANK_0;
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, __RW_MGR_LFSR_WR_RD_BANK_0_DATA);
		}		
		#endif
	}
	else
	{
		//USER CNTR 2 - In this case we want to execute the next instruction and NOT
		//USER take the jump. So we set the counter to 0. The jump address doesn't count
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_2, 0, 0x0);
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_2, 0, 0x0);

		//USER CNTR 3 - Set the nop counter to the number of cycles we need to loop for, minus 1
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_3, 0, gbl->rw_wl_nop_cycles - 1);
		if(test_dm)
		{
			mcc_instruction = __RW_MGR_LFSR_WR_RD_DM_BANK_0;
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_3, 0, __RW_MGR_LFSR_WR_RD_DM_BANK_0_NOP);
		}
		else
		{
			mcc_instruction = __RW_MGR_LFSR_WR_RD_BANK_0;
			IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_3, 0, __RW_MGR_LFSR_WR_RD_BANK_0_NOP);
		}
	}

	IOWR_32DIRECT (RW_MGR_RESET_READ_DATAPATH, 0, 0);

	if(quick_write_mode) {
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x08);
	} else {
		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x40);
	}
	IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, mcc_instruction);

	//USER CNTR 1 - This is used to ensure enough time elapses for read data to come back.
	//USER 16 clocks should be well enough.
	IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, 0x30);

	if(test_dm)
	{
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_LFSR_WR_RD_DM_BANK_0_WAIT);
	} else {
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_LFSR_WR_RD_BANK_0_WAIT);
	}

	IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, (group << 2), mcc_instruction);
}
#endif

//USER Test writes, can check for a single bit pass or multiple bit pass

alt_u32 rw_mgr_mem_calibrate_write_test (alt_u32 write_group, alt_u32 use_dm, alt_u32 all_correct, t_btfld *bit_chk)
{
	alt_u32 r;
	t_btfld correct_mask_vg;
	t_btfld tmp_bit_chk;
	alt_u32 vg;

	*bit_chk = param->write_correct_mask;
	correct_mask_vg = param->write_correct_mask_vg;

	for (r = 0; r < RW_MGR_MEM_NUMBER_OF_RANKS; r++) {
		if (param->skip_ranks[r]) {
			//USER request to skip the rank

			continue;
		}

		set_rank_and_odt_mask(r, RW_MGR_ODT_MODE_READ_WRITE);

		tmp_bit_chk = 0;
		for (vg = 0; vg < RW_MGR_MEM_VIRTUAL_GROUPS_PER_WRITE_DQS; vg++) {

			//USER reset the fifos to get pointers to known state 
			IOWR_32DIRECT (PHY_MGR_CMD_FIFO_RESET, 0, 0);			

			if (vg != 0) {
				tmp_bit_chk = tmp_bit_chk << (RW_MGR_MEM_DQ_PER_READ_DQS / RW_MGR_MEM_VIRTUAL_GROUPS_PER_READ_DQS);
			}
			rw_mgr_mem_calibrate_write_test_issue (write_group*RW_MGR_MEM_VIRTUAL_GROUPS_PER_WRITE_DQS+vg, use_dm);

			tmp_bit_chk = tmp_bit_chk | (correct_mask_vg & ~(IORD_32DIRECT(BASE_RW_MGR, 0)));
		}
		*bit_chk &= tmp_bit_chk;
	}

	if (all_correct)
	{
		set_rank_and_odt_mask(0, RW_MGR_ODT_MODE_OFF);
		return (*bit_chk == param->write_correct_mask);
	}
	else
	{
		set_rank_and_odt_mask(0, RW_MGR_ODT_MODE_OFF);
		return (*bit_chk != 0x00);
	}
}

//USER level the write operations

#if DYNAMIC_CALIBRATION_MODE || STATIC_QUICK_CALIBRATION

#if QDRII

//USER Write Levelling -- Quick Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{ return 0; }

#endif

#if RLDRAMII
#if !ENABLE_SUPER_QUICK_CALIBRATION

//USER Write Levelling -- Quick Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 d;
	t_btfld bit_chk;

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_WLEVEL);
	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		scc_mgr_apply_group_all_out_delay (g, test_bgn, d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}

	if (d > IO_IO_OUT1_DELAY_MAX) {
		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}

	return 1;
}

#else

//USER Write Levelling -- Super Quick Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 d;
	t_btfld bit_chk;

	//USER The first call to this function will calibrate all groups
	if (g != 0) {
		return 1;
	}

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_WLEVEL);
	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		scc_mgr_apply_group_all_out_delay (g, test_bgn, d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}

	if (d > IO_IO_OUT1_DELAY_MAX) {
		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}

	//USER Now copy the calibration settings to all other groups
	for (g = 1, test_bgn = RW_MGR_MEM_DQ_PER_WRITE_DQS; g < RW_MGR_MEM_IF_WRITE_DQS_WIDTH; g++, test_bgn += RW_MGR_MEM_DQ_PER_WRITE_DQS) {
		scc_mgr_apply_group_all_out_delay (g, test_bgn, d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Verify that things worked as expected
		if (!rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			return 0;
		}
	}

	return 1;
}

#endif
#endif

#if DDRX
#if !ENABLE_SUPER_QUICK_CALIBRATION

//USER Write Levelling -- Quick Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 p;
	t_btfld bit_chk;

	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_WLEVEL);

	//USER maximum phases for the sweep 

	//USER starting phases 

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	for (p = 0; p <= IO_DQDQS_OUT_PHASE_MAX; p++) {
		WRITE_SCC_DQDQS_OUT_PHASE(g, p);
		scc_mgr_load_dqs (g);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}

	if (p > IO_DQDQS_OUT_PHASE_MAX) {
		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}

	return 1;
}

#else

//USER Write Levelling -- Super Quick Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 p;
	t_btfld bit_chk;

	//USER The first call to this function will calibrate all groups
	if (g != 0) {
		return 1;
	}

	//USER update info for sims 

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_WLEVEL);

	//USER maximum phases for the sweep 

	//USER starting phases 

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	for (p = 0; p <= IO_DQDQS_OUT_PHASE_MAX; p++) {
		WRITE_SCC_DQDQS_OUT_PHASE(g, p);
		scc_mgr_load_dqs (g);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}

	if (p > IO_DQDQS_OUT_PHASE_MAX) {
		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}

	//USER Now copy the calibration settings to all other groups
	for (g = 1, test_bgn = RW_MGR_MEM_DQ_PER_READ_DQS; (g < RW_MGR_MEM_IF_READ_DQS_WIDTH); g++, test_bgn += RW_MGR_MEM_DQ_PER_READ_DQS) {
		WRITE_SCC_DQDQS_OUT_PHASE(g, p);
		scc_mgr_load_dqs (g);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Verify that things worked as expected
		if (!rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			return 0;
		}
	}

	return 1;
}

#endif
#endif

#endif

#if DYNAMIC_CALIBRATION_MODE || STATIC_FULL_CALIBRATION

#if QDRII 
//USER Write Levelling -- Full Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{return 0;}
#endif

#if RLDRAMII
//USER Write Levelling -- Full Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 d;
	t_btfld bit_chk;
	alt_u32 work_bgn, work_end;
	alt_u32 d_bgn, d_end;
	alt_u32 found_begin;

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_WLEVEL);

	//USER maximum delays for the sweep 

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	//USER starting and end range where writes work 

	work_bgn = 0;
	work_end = 0;

	//USER step 1: find first working dtap, increment in dtaps
	found_begin = 0;
	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX; d++, work_bgn += IO_DELAY_PER_DCHAIN_TAP) {
		scc_mgr_apply_group_all_out_delay (g, test_bgn, d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
		
		if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			found_begin = 1;
			d_bgn = d;
			break;
		}
		
	}

	if (!found_begin) {
		//USER fail, cannot find first working delay

		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}
	
	//USER step 2 : find first non-working dtap, increment in dtaps
	work_end = work_bgn;
	d = d + 1;
	for (; d <= IO_IO_OUT1_DELAY_MAX; d++, work_end += IO_DELAY_PER_DCHAIN_TAP) {
		scc_mgr_apply_group_all_out_delay (g, test_bgn, d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}
	d_end = d - 1;

	if (d_end >= d_bgn) {
		//USER we have a working range 
	} else {
		//USER nil range 

		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}

	//USER export write leveling windows to TCL interface 

	gbl->export_dqdqs_bgn[g] = work_bgn;
	gbl->export_dqdqs_end[g] = work_end;

	//USER center 

	d = (d_end + d_bgn) / 2;

	scc_mgr_apply_group_all_out_delay(g, test_bgn, 0);
	scc_mgr_apply_group_all_out_delay_add (g, test_bgn, d);

	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);


	return 1;
}
#endif


#if DDRX
#if NEWVERSION_WL

//USER Write Levelling -- Full Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 p, d;
	t_btfld bit_chk;
	alt_u32 work_bgn, work_end, work_mid;
	alt_u32 tmp_delay;
	alt_u32 found_begin;
	alt_u32 dtaps_per_ptap;	

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_WLEVEL);

	//USER maximum phases for the sweep 

	dtaps_per_ptap = 0;
	tmp_delay = 0;
	while (tmp_delay < IO_DELAY_PER_OPA_TAP) {
		dtaps_per_ptap++;
		tmp_delay += IO_DELAY_PER_DCHAIN_TAP;
	}
	dtaps_per_ptap--;
	tmp_delay = 0;	

	//USER starting phases 

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	//USER starting and end range where writes work 

	work_bgn = 0;
	work_end = 0;

	//USER step 1: find first working phase, increment in ptaps, and then in dtaps if ptaps doesn't find a working phase 
	found_begin = 0;
	tmp_delay = 0;
	for (d = 0; d <= dtaps_per_ptap; d++, tmp_delay += IO_DELAY_PER_DCHAIN_TAP) {
		scc_mgr_apply_group_all_out_delay (g, test_bgn, d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
		work_bgn = tmp_delay;
		
		for (p = 0; p <= IO_DQDQS_OUT_PHASE_MAX; p++, work_bgn += IO_DELAY_PER_OPA_TAP) {
			WRITE_SCC_DQDQS_OUT_PHASE(g, p);
			scc_mgr_load_dqs (g);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
				found_begin = 1;
				break;
			}
		}
		
		if (found_begin) {
			break;
		}
	}

	if (p > IO_DQDQS_OUT_PHASE_MAX) {
		//USER fail, cannot find first working phase 

		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}
	
	//USER If d is 0 then the working window covers a phase tap and we can follow the old procedure
	//USER 	otherwise, we've found the beginning, and we need to increment the dtaps until we find the end 
	if (d == 0) {
		work_end = work_bgn + IO_DELAY_PER_OPA_TAP;

		//USER step 2: if we have room, back off by one and increment in dtaps 
		
		if (p > 0) {
			WRITE_SCC_DQDQS_OUT_PHASE(g, p - 1);
			scc_mgr_load_dqs (g);

			tmp_delay = work_bgn - IO_DELAY_PER_OPA_TAP;

			for (d = 0; d <= IO_IO_OUT1_DELAY_MAX && tmp_delay < work_bgn; d++, tmp_delay += IO_DELAY_PER_DCHAIN_TAP) {
				scc_mgr_apply_group_all_out_delay (g, test_bgn, d);

				IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

				if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
					work_bgn = tmp_delay;
					break;
				}
			}

			scc_mgr_apply_group_all_out_delay (g, test_bgn, 0);
		}

		//USER step 3: go forward from working phase to non working phase, increment in ptaps 

		for (p = p + 1; p <= IO_DQDQS_OUT_PHASE_MAX; p++, work_end += IO_DELAY_PER_OPA_TAP) {
			WRITE_SCC_DQDQS_OUT_PHASE(g, p);
			scc_mgr_load_dqs (g);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			if (!rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
				break;
			}
		}

		//USER step 4: back off one from last, increment in dtaps 
		//USER The actual increment is done outside the if/else statement since it is shared with other code

		WRITE_SCC_DQDQS_OUT_PHASE(g, p - 1);
		scc_mgr_load_dqs (g);

		work_end -= IO_DELAY_PER_OPA_TAP;
		d = 0;

	} else {
		//USER step 5: Window doesn't cover phase tap, just increment dtaps until failure
		//USER The actual increment is done outside the if/else statement since it is shared with other code
		work_end = work_bgn;

	}
	
	//USER The actual increment until failure
	for (; d <= IO_IO_OUT1_DELAY_MAX; d++, work_end += IO_DELAY_PER_DCHAIN_TAP) {
		scc_mgr_apply_group_all_out_delay (g, test_bgn, d);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}
	scc_mgr_apply_group_all_out_delay (g, test_bgn, 0);		

	if (work_end > work_bgn) {
		//USER we have a working range 
	} else {
		//USER nil range 

		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}

	//USER export write leveling windows to TCL interface 

	gbl->export_dqdqs_bgn[g] = work_bgn;
	gbl->export_dqdqs_end[g] = work_end;

	//USER center 

	work_mid = (work_bgn + work_end) / 2;

	tmp_delay = 0;

	for (p = 0; p <= IO_DQDQS_OUT_PHASE_MAX && tmp_delay < work_mid; p++, tmp_delay += IO_DELAY_PER_OPA_TAP);

	tmp_delay -= IO_DELAY_PER_OPA_TAP;

	WRITE_SCC_DQDQS_OUT_PHASE(g, p - 1);
	scc_mgr_load_dqs (g);

	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX && tmp_delay < work_mid; d++, tmp_delay += IO_DELAY_PER_DCHAIN_TAP);

	scc_mgr_apply_group_all_out_delay_add (g, test_bgn, d - 1);

	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	return 1;
}


#else

//USER Write Levelling -- Full Calibration
alt_u32 rw_mgr_mem_calibrate_wlevel (alt_u32 g, alt_u32 test_bgn)
{
	alt_u32 p, d;
	t_btfld bit_chk;
	alt_u32 work_bgn, work_end, work_mid;
	alt_u32 tmp_delay;

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_WLEVEL);

	//USER maximum phases for the sweep 

	//USER starting phases 

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	//USER starting and end range where writes work 

	work_bgn = 0;
	work_end = 0;

	//USER step 1: find first working phase, increment in ptaps 

	for (p = 0; p <= IO_DQDQS_OUT_PHASE_MAX; p++, work_bgn += IO_DELAY_PER_OPA_TAP) {
		WRITE_SCC_DQDQS_OUT_PHASE(g, p);
		scc_mgr_load_dqs (g);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}

	if (p > IO_DQDQS_OUT_PHASE_MAX) {
		//USER fail, cannot find first working phase 

		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}

	work_end = work_bgn + IO_DELAY_PER_OPA_TAP;

	//USER step 2: if we have room, back off by one and increment in dtaps 

	if (p > 0) {
		WRITE_SCC_DQDQS_OUT_PHASE(g, p - 1);
		scc_mgr_load_dqs (g);

		tmp_delay = work_bgn - IO_DELAY_PER_OPA_TAP;

		for (d = 0; d <= IO_IO_OUT1_DELAY_MAX && tmp_delay < work_bgn; d++, tmp_delay += IO_DELAY_PER_DCHAIN_TAP) {
			scc_mgr_apply_group_all_out_delay (g, test_bgn, d);

			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			if (rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
				work_bgn = tmp_delay;
				break;
			}
		}

		scc_mgr_apply_group_all_out_delay (g, test_bgn, 0);
	}

	//USER step 3: go forward from working phase to non working phase, increment in ptaps 

	for (p = p + 1; p <= IO_DQDQS_OUT_PHASE_MAX; p++, work_end += IO_DELAY_PER_OPA_TAP) {
		WRITE_SCC_DQDQS_OUT_PHASE(g, p);
		scc_mgr_load_dqs (g);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}

	//USER step 4: back off one from last, increment in dtaps 

	WRITE_SCC_DQDQS_OUT_PHASE(g, p - 1);
	scc_mgr_load_dqs (g);

	work_end -= IO_DELAY_PER_OPA_TAP;

	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX; d++, work_end += IO_DELAY_PER_DCHAIN_TAP) {
		scc_mgr_apply_group_all_out_delay (g, test_bgn, d);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_write_test (g, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		}
	}

	scc_mgr_apply_group_all_out_delay (g, test_bgn, 0);

	if (work_end > work_bgn) {
		//USER we have a working range 
	} else {
		//USER nil range 

		gbl->error_stage = CAL_STAGE_WLEVEL;
		gbl->error_group = g;

		return 0;
	}

	//USER export write leveling windows to TCL interface 

	gbl->export_dqdqs_bgn[g] = work_bgn;
	gbl->export_dqdqs_end[g] = work_end;

	//USER center 

	work_mid = (work_bgn + work_end) / 2;

	tmp_delay = 0;

	for (p = 0; p <= IO_DQDQS_OUT_PHASE_MAX && tmp_delay < work_mid; p++, tmp_delay += IO_DELAY_PER_OPA_TAP);

	tmp_delay -= IO_DELAY_PER_OPA_TAP;

	WRITE_SCC_DQDQS_OUT_PHASE(g, p - 1);
	scc_mgr_load_dqs (g);

	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX && tmp_delay < work_mid; d++, tmp_delay += IO_DELAY_PER_DCHAIN_TAP);

	scc_mgr_apply_group_all_out_delay_add (g, test_bgn, d - 1);

	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);


	return 1;
}

#endif
#endif
#endif

//USER center all windows. do per-bit-deskew to possibly increase size of certain windows
	
#if RLDRAMII
//USER For RLDRAM there is one DM pin per interface, so one DM per multiple write groups
//USER so we need to remember the left and right edge of the window over many writes deskew
//USER calls, and then center over the whole interface.  We do this by storing the worst
//USER case edges in these two global variables.
alt_32 dm_left_edge;
alt_32 dm_right_edge;
#endif
	
#if NEWVERSION_WRDESKEW

alt_u32 rw_mgr_mem_calibrate_writes_center (alt_u32 write_group, alt_u32 test_bgn)
{
	alt_u32 i, p, d, min_index;
	//USER Store these as signed since there are comparisons with signed numbers
	t_btfld bit_chk;
	t_btfld sticky_bit_chk;
	alt_32 left_edge[RW_MGR_MEM_DQ_PER_WRITE_DQS];
	alt_32 right_edge[RW_MGR_MEM_DQ_PER_WRITE_DQS];
	alt_32 mid;
	alt_32 mid_min, orig_mid_min;
	alt_32 new_dqs, start_dqs, shift_dq;
	alt_u32 dq_margin, dqs_margin, dm_margin;
	alt_u32 stop;
#if RLDRAMII
	alt_32 decrement_counter;
#endif

	start_dqs = READ_SCC_DQS_IO_OUT1_DELAY();

	//USER per-bit deskew 
		
	//USER set the left and right edge of each bit to an illegal value 
	//USER use (IO_IO_OUT1_DELAY_MAX + 1) as an illegal value
	sticky_bit_chk = 0;
	for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {
		left_edge[i]  = IO_IO_OUT1_DELAY_MAX + 1;
		right_edge[i] = IO_IO_OUT1_DELAY_MAX + 1;
	}
	
	//USER Search for the left edge of the window for each bit
	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		scc_mgr_apply_group_dq_out1_delay (test_bgn, d);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Stop searching when the read test doesn't pass AND when we've seen a passing read on every bit 
		stop = !rw_mgr_mem_calibrate_write_test (write_group, 0, PASS_ONE_BIT, &bit_chk);
		sticky_bit_chk = sticky_bit_chk | bit_chk;
		stop = stop && (sticky_bit_chk == param->read_correct_mask);
		
		if (stop == 1) {
			break;
		} else {
			for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {
				if (bit_chk & 1) {
					//USER Remember a passing test as the left_edge
					left_edge[i] = d;
				} else {
					//USER If a left edge has not been seen yet, then a future passing test will mark this edge as the right edge 
					if (left_edge[i] == IO_IO_OUT1_DELAY_MAX + 1) {
						right_edge[i] = -(d + 1);
					}
				}
				bit_chk = bit_chk >> 1;
			}
		}
	}

	//USER Reset DQ delay chains to 0 
	scc_mgr_apply_group_dq_out1_delay (test_bgn, 0);
	sticky_bit_chk = 0;
	for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {

		//USER Check for cases where we haven't found the left edge, which makes our assignment of the the 
		//USER right edge invalid.  Reset it to the illegal value. 
		if ((left_edge[i] == IO_IO_OUT1_DELAY_MAX + 1) && (right_edge[i] != IO_IO_OUT1_DELAY_MAX + 1)) {
			right_edge[i] = IO_IO_OUT1_DELAY_MAX + 1;
		}
		
		//USER Reset sticky bit (except for bits where we have seen both the left and right edge) 
		sticky_bit_chk = sticky_bit_chk << 1;
		if ((left_edge[i] != IO_IO_OUT1_DELAY_MAX + 1) && (right_edge[i] != IO_IO_OUT1_DELAY_MAX + 1)) {
			sticky_bit_chk = sticky_bit_chk | 1;
		}
	}
	
	//USER Search for the right edge of the window for each bit 
	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX - start_dqs; d++) {
		scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, d + start_dqs);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
		if (QDRII)
		{
			rw_mgr_mem_dll_lock_wait();
		}

		//USER Stop searching when the read test doesn't pass AND when we've seen a passing read on every bit 
		stop = !rw_mgr_mem_calibrate_write_test (write_group, 0, PASS_ONE_BIT, &bit_chk);
		sticky_bit_chk = sticky_bit_chk | bit_chk;
		stop = stop && (sticky_bit_chk == param->read_correct_mask);
		
		if (stop == 1) {
			break;
		} else {
			for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {
				if (bit_chk & 1) {
					//USER Remember a passing test as the right_edge 
					right_edge[i] = d;
				} else {
					if (d != 0) {
						//USER If a right edge has not been seen yet, then a future passing test will mark this edge as the left edge 
						if (right_edge[i] == IO_IO_OUT1_DELAY_MAX + 1) {
							left_edge[i] = -(d + 1);
						}
					} else {
						//USER d = 0 failed, but it passed when testing the left edge, so it must be marginal, set it to -1
						if (right_edge[i] == IO_IO_OUT1_DELAY_MAX + 1) {
							right_edge[i] = -1;
						}
					}
				}
				bit_chk = bit_chk >> 1;
			}
		}
	}
	
	//USER Check that all bits have a window
	for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {
		if ((left_edge[i] == IO_IO_OUT1_DELAY_MAX + 1) || (right_edge[i] == IO_IO_OUT1_DELAY_MAX + 1)) {
			gbl->error_stage = CAL_STAGE_WRITES;
			gbl->error_group = test_bgn + i;
			return 0;
		}
	}		
	
	//USER Find middle of window for each DQ bit 
	mid_min = left_edge[0] - right_edge[0];
	min_index = 0;
	for (i = 1; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {
		mid = left_edge[i] - right_edge[i];
		if (mid < mid_min) {
			mid_min = mid;
			min_index = i;
		}
	}
	//USER  -mid_min/2 represents the amount that we need to move DQS.  If mid_min is odd we'll need to add one to make sure 
	//USER 	the rounding in further calculations is correct 
	mid_min = (mid_min+1)/2;	

	//USER Determine the amount we can change DQS (which is -mid_min)
	orig_mid_min = mid_min;		
#if ENABLE_DQS_OUT_CENTERING
	if (DDRX) {
		new_dqs = start_dqs - mid_min;
		if (new_dqs > IO_IO_OUT1_DELAY_MAX) {
			new_dqs = IO_IO_OUT1_DELAY_MAX;
		} else if (new_dqs < 0) {
			new_dqs = 0;
		} 
		mid_min = start_dqs - new_dqs;

		new_dqs = start_dqs - mid_min;
	}
#else
	new_dqs = start_dqs;
	mid_min = 0;
#endif
	
	//USER Initialize data for export structures 
	dqs_margin = IO_IO_OUT1_DELAY_MAX + 1;
	dq_margin  = IO_IO_OUT1_DELAY_MAX + 1;
	
	//USER add delay to bring centre of all DQ windows to the same "level" 
	for (i = 0, p = test_bgn; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++, p++) {
		//USER Use values before divide by 2 to reduce round off error 
		shift_dq = (left_edge[i] - right_edge[i] - (left_edge[min_index] - right_edge[min_index]))/2  + (orig_mid_min - mid_min);
		
		if (shift_dq + (alt_32)READ_SCC_DQ_OUT1_DELAY(i) > (alt_32)IO_IO_OUT1_DELAY_MAX) {
			shift_dq = (alt_32)IO_IO_OUT1_DELAY_MAX - READ_SCC_DQ_OUT1_DELAY(i);
		} else if (shift_dq + (alt_32)READ_SCC_DQ_OUT1_DELAY(i) < 0) {
			shift_dq = -(alt_32)READ_SCC_DQ_OUT1_DELAY(i);
		} 
		WRITE_SCC_DQ_OUT1_DELAY(i, READ_SCC_DQ_OUT1_DELAY(i) + shift_dq);
		scc_mgr_load_dq (p, i);
		
		//USER To determine values for export structures 
		if (left_edge[i] - shift_dq + (-mid_min) < dq_margin) {
			dq_margin = left_edge[i] - shift_dq + (-mid_min);
		}
		if (right_edge[i] + shift_dq - (-mid_min) < dqs_margin) {
			dqs_margin = right_edge[i] + shift_dq - (-mid_min);
		}
	}

	//USER Move DQS 
	scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, new_dqs);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);


	//////////////////////
	//////////////////////
	//USER Centre DM 
	//////////////////////
	//////////////////////

#if RLDRAMII
	//USER Determine if first group in device to initialize left and right edges
	decrement_counter = write_group;
	while (decrement_counter > 0) {
		decrement_counter -= RW_MGR_MEM_IF_WRITE_DQS_WIDTH;
	}
	if (decrement_counter == 0) {
		dm_left_edge  = IO_IO_OUT1_DELAY_MAX + 1;
		dm_right_edge = IO_IO_OUT1_DELAY_MAX + 1;
	}


	sticky_bit_chk = 0;
	//USER Search for the left edge of the window for the DM bit
	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		scc_mgr_apply_group_dm_out1_delay (d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Stop searching when the read test doesn't pass for all bits (as they've already been calibrated)
		if (rw_mgr_mem_calibrate_write_test (write_group, 1, PASS_ALL_BITS, &bit_chk)) {
			left_edge[0] = d;
		} else {
			break;
		}
	}

	//USER Reset DM delay chains to 0
	scc_mgr_apply_group_dm_out1_delay (0);

	sticky_bit_chk = 0;
	//USER Search for the right edge of the window for the DM bit
	for (d = 1; d <= IO_IO_OUT1_DELAY_MAX - new_dqs; d++) {
		scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, d + new_dqs);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Stop searching when the read test doesn't pass for all bits (as they've already been calibrated)
		if (rw_mgr_mem_calibrate_write_test (write_group, 1, PASS_ALL_BITS, &bit_chk)) {
			right_edge[0] = d;
		} else {
			right_edge[0] = d - 1;
			break;
		}
	}

	//USER Find intersection of left and right DM edges over all groups
	if (right_edge[0] < dm_right_edge) {
		dm_right_edge = right_edge[0];
	}
	if (left_edge[0] < dm_left_edge) {
		dm_left_edge = left_edge[0];
	}


	//USER Move DQS (back to orig)
	scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, new_dqs);

	//USER move DM

	//USER Determine if last group in device
	decrement_counter = write_group + 1;
	while (decrement_counter > 0) {
		decrement_counter -= RW_MGR_MEM_IF_WRITE_DQS_WIDTH;
	}
	if (decrement_counter == 0) {
		//USER Find middle of window for the DM bit
		mid = (dm_left_edge - dm_right_edge) / 2;
		if (mid < 0) {
			mid = 0;
		}
		scc_mgr_apply_group_dm_out1_delay (mid);
	} else {
		//USER Set to DM back to original value for other groups, ready for testing
		scc_mgr_apply_group_dm_out1_delay (0);
	}
	dm_margin = dm_left_edge;
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
#endif


#if DDRX
	sticky_bit_chk = 0;

	//USER Search for the left edge of the window for the DM bit
	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		scc_mgr_apply_group_dm_out1_delay (d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Stop searching when the read test doesn't pass for all bits (as they've already been calibrated)
		if (rw_mgr_mem_calibrate_write_test (write_group, 1, PASS_ALL_BITS, &bit_chk)) {
			left_edge[0] = d;
		} else {
			break;
		}
	}

	//USER Reset DM delay chains to 0
	scc_mgr_apply_group_dm_out1_delay (0);

	sticky_bit_chk = 0;

	//USER Search for the right edge of the window for the DM bit
	for (d = 1; d <= IO_IO_OUT1_DELAY_MAX - new_dqs; d++) {
		scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, d + new_dqs);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Stop searching when the read test doesn't pass for all bits (as they've already been calibrated)
		if (rw_mgr_mem_calibrate_write_test (write_group, 1, PASS_ALL_BITS, &bit_chk)) {
			right_edge[0] = d;
		} else {
			right_edge[0] = d - 1;
			break;
		}
	}

	//USER Move DQS (back to orig)
	scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, new_dqs);

	//USER Move DM

	//USER Find middle of window for the DM bit
	mid = (left_edge[0] - right_edge[0]) / 2;
	if (mid < 0) {
		mid = 0;
	}
	scc_mgr_apply_group_dm_out1_delay(mid);
	dm_margin = left_edge[0];
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
#endif

#if QDRII
	sticky_bit_chk = 0;

	//USER Search for the left edge of the window for the DM bit
	for (d = 0; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		scc_mgr_apply_group_dm_out1_delay (d);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		//USER Stop searching when the read test doesn't pass for all bits (as they've already been calibrated)
		stop = !rw_mgr_mem_calibrate_write_test (write_group, 1, PASS_ALL_BITS, &bit_chk);
		sticky_bit_chk = sticky_bit_chk | bit_chk;
		stop = stop && (sticky_bit_chk == param->read_correct_mask);

		if (stop == 1) {
			break;
		} else {
			for (i = 0; i < RW_MGR_MEM_DATA_MASK_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH; i++) {
				if ((bit_chk & param->dm_correct_mask) == param->dm_correct_mask) {
					left_edge[i] = d;
				}
				bit_chk = bit_chk >> (RW_MGR_MEM_DATA_WIDTH / RW_MGR_MEM_DATA_MASK_WIDTH);
			}
		}
	}

	//USER Reset DM delay chains to 0
	scc_mgr_apply_group_dm_out1_delay (0);

	sticky_bit_chk = 0;

	//USER Search for the right edge of the window for the DM bit
	for (d = 1; d <= IO_IO_OUT1_DELAY_MAX - new_dqs; d++) {
		scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, d + new_dqs);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		rw_mgr_mem_dll_lock_wait();

		//USER Stop searching when the read test doesn't pass for all bits (as they've already been calibrated)
		stop = !rw_mgr_mem_calibrate_write_test (write_group, 1, PASS_ALL_BITS, &bit_chk);
		sticky_bit_chk = sticky_bit_chk | bit_chk;
		stop = stop && (sticky_bit_chk == param->read_correct_mask);

		if (stop == 1) {
			right_edge[i] = d - 1;
			break;
		} else {
			for (i = 0; i < RW_MGR_MEM_DATA_MASK_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH; i++) {
				if ((bit_chk & param->dm_correct_mask) == param->dm_correct_mask) {
					right_edge[i] = d;
				}
				bit_chk = bit_chk >> (RW_MGR_MEM_DATA_WIDTH / RW_MGR_MEM_DATA_MASK_WIDTH);
			}
		}
	}

	//USER Move DQS (back to orig)
	scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, new_dqs);

	//USER Move DM
	dm_margin = IO_IO_OUT1_DELAY_MAX;
	for (i = 0; i < RW_MGR_MEM_DATA_MASK_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH; i++) {
		//USER Find middle of window for the DM bit
		mid = (left_edge[i] - right_edge[i]) / 2;
		if (mid < 0) {
			mid = 0;
		}
		WRITE_SCC_DM_IO_OUT1_DELAY(i, mid);
		scc_mgr_load_dm (i);
		if (left_edge[i] < dm_margin) {
			dm_margin = left_edge[i];
		}
	}
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);


#endif


	//USER Export values 
	gbl->fom_out += dq_margin + dqs_margin;
	gbl->export_dqso_margin[write_group] = dqs_margin;
	gbl->export_dqo_margin[write_group] = dq_margin;
	gbl->export_dmo_margin[write_group] = dm_margin;

	return (dq_margin + dqs_margin) > 0;
}

#else

alt_u32 rw_mgr_mem_calibrate_writes_center (alt_u32 write_group, alt_u32 test_bgn)
{
	alt_u32 i, p, d;
	alt_u32 mid;
	t_btfld bit_chk, sticky_bit_chk;
	alt_u32 max_working_dq[RW_MGR_MEM_DQ_PER_WRITE_DQS];
	alt_u32 max_working_dm[RW_MGR_MEM_DATA_MASK_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH];
	alt_u32 dq_margin, dqs_margin, dm_margin;
	alt_u32 start_dqs;
	alt_u32 stop;

	//USER per-bit deskew 

	for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {
		max_working_dq[i] = 0;
	}

	for (d = 1; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		scc_mgr_apply_group_dq_out1_delay (test_bgn, d);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_write_test (write_group, 0, PASS_ONE_BIT, &bit_chk)) {
			break;
		} else {
			for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {
				if (bit_chk & 1) {
					max_working_dq[i] = d;
				}
				bit_chk = bit_chk >> 1;
			}
		}
	}

	scc_mgr_apply_group_dq_out1_delay (test_bgn, 0);

	//USER determine minimum of maximums 

	dq_margin = IO_IO_OUT1_DELAY_MAX;

	for (i = 0; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++) {
		if (max_working_dq[i] < dq_margin) {
			dq_margin = max_working_dq[i];
		}
	}

	//USER add delay to center DQ windows 

	for (i = 0, p = test_bgn; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++, p++) {
		if (max_working_dq[i] > dq_margin) {
			WRITE_SCC_DQ_OUT1_DELAY(i, max_working_dq[i] - dq_margin);
		} else {
			WRITE_SCC_DQ_OUT1_DELAY(i, 0);
		}

		scc_mgr_load_dq (p, i);
	}

	//USER sweep DQS window, may potentially have more window due to per-bit-deskew

	start_dqs = READ_SCC_DQS_IO_OUT1_DELAY();

	for (d = start_dqs + 1; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		scc_mgr_apply_group_dqs_io_and_oct_out1 (write_group, d);

		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (QDRII)
		{
			rw_mgr_mem_dll_lock_wait();
		}		

		if (!rw_mgr_mem_calibrate_write_test (write_group, 0, PASS_ALL_BITS, &bit_chk)) {
			break;
		}
	}

	WRITE_SCC_DQS_IO_OUT1_DELAY(start_dqs);
	WRITE_SCC_OCT_OUT1_DELAY(write_group, start_dqs);

	dqs_margin = d - start_dqs - 1;

	//USER time to center, +1

	mid = (dq_margin + dqs_margin + 1) / 2;

	gbl->fom_out += dq_margin + dqs_margin;

#if ENABLE_DQS_OUT_CENTERING
	//USER center DQS ... if the headroom is setup properly we shouldn't need to 
	if (DDRX) {
		if (dqs_margin > mid) {
			WRITE_SCC_DQS_IO_OUT1_DELAY(READ_SCC_DQS_IO_OUT1_DELAY() + dqs_margin - mid);
			WRITE_SCC_OCT_OUT1_DELAY(write_group, READ_SCC_OCT_OUT1_DELAY(write_group) + dqs_margin - mid);
		}
	}
#endif

	scc_mgr_load_dqs_io ();
	scc_mgr_load_dqs (write_group);

	//USER center dq 

	if (dq_margin > mid) {
		for (i = 0, p = test_bgn; i < RW_MGR_MEM_DQ_PER_WRITE_DQS; i++, p++) {
			WRITE_SCC_DQ_OUT1_DELAY(i, READ_SCC_DQ_OUT1_DELAY(i) + dq_margin - mid);
			scc_mgr_load_dq (p, i);
		}
		dqs_margin += dq_margin - mid;
		dq_margin  -= dq_margin - mid;
	}

	//USER do dm centering 

	if (!RLDRAMII) {
		dm_margin = IO_IO_OUT1_DELAY_MAX;

		if (QDRII) {
			sticky_bit_chk = 0;
			for (i = 0; i < RW_MGR_MEM_DATA_MASK_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH; i++) {
				max_working_dm[i] = 0;
			}
		}

		for (d = 1; d <= IO_IO_OUT1_DELAY_MAX; d++) {
			scc_mgr_apply_group_dm_out1_delay (d);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

			if (DDRX) {
				if (rw_mgr_mem_calibrate_write_test (write_group, 1, PASS_ALL_BITS, &bit_chk)) {
					max_working_dm[0] = d;
				} else {
					break;
				}
			} else {
				stop = !rw_mgr_mem_calibrate_write_test (write_group, 1, PASS_ALL_BITS, &bit_chk);
				sticky_bit_chk = sticky_bit_chk | bit_chk;
				stop = stop && (sticky_bit_chk == param->read_correct_mask);

				if (stop == 1) {
					break;
				} else {
					for (i = 0; i < RW_MGR_MEM_DATA_MASK_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH; i++) {
						if ((bit_chk & param->dm_correct_mask) == param->dm_correct_mask) {
							max_working_dm[i] = d;
						}
						bit_chk = bit_chk >> (RW_MGR_MEM_DATA_WIDTH / RW_MGR_MEM_DATA_MASK_WIDTH);
					}
				}
			}
		}

		i = 0;
		for (i = 0; i < RW_MGR_NUM_DM_PER_WRITE_GROUP; i++) {
			if (max_working_dm[i] > mid) {
				WRITE_SCC_DM_IO_OUT1_DELAY(i, max_working_dm[i] - mid);
			} else {
				WRITE_SCC_DM_IO_OUT1_DELAY(i, 0);
			}

			scc_mgr_load_dm (i);

			if (max_working_dm[i] < dm_margin) {
				dm_margin = max_working_dm[i];
			}
		}
	} else {
		dm_margin = 0;
	}

	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	//USER save in export structures 

	gbl->export_dqso_margin[write_group] = dqs_margin;
	gbl->export_dqo_margin[write_group] = dq_margin;
	gbl->export_dmo_margin[write_group] = dm_margin;

	return (dq_margin + dqs_margin) > 0;
}

#endif

//USER calibrate the write operations

alt_u32 rw_mgr_mem_calibrate_writes (alt_u32 g, alt_u32 test_bgn)
{
	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_WRITES);

	//USER starting phases 

	//USER update info for sims

	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, g);

	if (!rw_mgr_mem_calibrate_writes_center (g, test_bgn)) {
		gbl->error_stage = CAL_STAGE_WRITES;
		gbl->error_group = g;
		return 0;
	}


	return 1;
}


void rw_mgr_mem_calibrate_eye_diag_aid (void)
{
	
}


alt_u32 rw_mgr_mem_calibrate_full_test (alt_u32 min_correct, t_btfld *bit_chk, alt_u32 test_dm)
{
	alt_u32 g;
	alt_u32 success;
	alt_u32 run_groups = ~param->skip_groups;

	for (g = 0; g < RW_MGR_MEM_IF_READ_DQS_WIDTH; g++) {
		if (run_groups & ((1 << RW_MGR_NUM_DQS_PER_WRITE_GROUP) - 1))
		{
			success = rw_mgr_mem_calibrate_write_test (g, test_dm, PASS_ALL_BITS, bit_chk);
		}
		run_groups = run_groups >> RW_MGR_NUM_DQS_PER_WRITE_GROUP;
	}

	return success;
}


void full_test_dq (alt_u32 dq, volatile alt_u32 *payload)
{
	alt_u32 d;
	alt_u32 start;
	alt_u32 working_cnt;
	t_btfld bit_chk;

	start = READ_SCC_DQ_IN_DELAY(dq);
	working_cnt = 0;

	for (d = start; d <= IO_IO_IN_DELAY_MAX; d++) {
		WRITE_SCC_DQ_IN_DELAY(dq, d);
		scc_mgr_load_dq (dq, dq);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_full_test (RW_MGR_MEM_DATA_WIDTH, &bit_chk, 0)) {
			break;
		}

		working_cnt++;
	}

	WRITE_SCC_DQ_IN_DELAY(dq, start);
	scc_mgr_load_dq (dq, dq);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	payload[0] = working_cnt;

	start = READ_SCC_DQ_OUT1_DELAY(dq);
	working_cnt = 0;

	for (d = start; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		WRITE_SCC_DQ_OUT1_DELAY(dq, d);
		scc_mgr_load_dq (dq, dq);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_full_test (RW_MGR_MEM_DATA_WIDTH, &bit_chk, 0)) {
			break;
		}

		working_cnt++;
	}

	WRITE_SCC_DQ_OUT1_DELAY(dq, start);
	scc_mgr_load_dq (dq, dq);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

	payload[1] = working_cnt;
}

void full_test_dqs (alt_u32 dqs, volatile alt_u32 *payload)
{
	alt_u32 d;
	alt_u32 start;
	alt_u32 working_cnt;
	t_btfld bit_chk;
	alt_u32 read_group;
	alt_u32 i;

	for (read_group = dqs * RW_MGR_MEM_IF_READ_DQS_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH, i = 0;
		read_group < (dqs + 1) * RW_MGR_MEM_IF_READ_DQS_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH;
		read_group++, i++) {	
		
		start = READ_SCC_DQS_IN_DELAY(read_group);
		working_cnt = 0;
		
		for (d = start; d <= IO_DQS_IN_DELAY_MAX; d++) {
			WRITE_SCC_DQS_IN_DELAY(read_group, d);
			scc_mgr_load_dqs (read_group);
			IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
	
			if (!rw_mgr_mem_calibrate_full_test (RW_MGR_MEM_DATA_WIDTH, &bit_chk, 0)) {
				break;
			}
	
			working_cnt++;
		}
	
		WRITE_SCC_DQS_IN_DELAY(dqs, start);
		scc_mgr_load_dqs (dqs);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
	
		if (i == 0) {
			payload[0] = working_cnt;
		} else {
			payload[i+1] = working_cnt;
		}
	}

	start = READ_SCC_DQS_IO_OUT1_DELAY();
	working_cnt = 0;

	for (d = start; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		WRITE_SCC_DQS_IO_OUT1_DELAY(d);
		scc_mgr_load_dqs_io ();
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);


		if (!rw_mgr_mem_calibrate_full_test (RW_MGR_MEM_DATA_WIDTH, &bit_chk, 0)) {
			break;
		}

		working_cnt++;
	}

	payload[1] = working_cnt;

	WRITE_SCC_DQS_IO_OUT1_DELAY(start);
	scc_mgr_load_dqs_io ();
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
}


void full_test_dm (alt_u32 dm, volatile alt_u32 *payload)
{
	alt_u32 d;
	alt_u32 start;
	alt_u32 working_cnt;
	t_btfld bit_chk;

	//USER sweep output delays 

	start = READ_SCC_DM_IO_OUT1_DELAY(dm);
	working_cnt = 0;

	for (d = start; d <= IO_IO_OUT1_DELAY_MAX; d++) {
		WRITE_SCC_DM_IO_OUT1_DELAY(dm, d);
		scc_mgr_load_dm (dm);
		IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);

		if (!rw_mgr_mem_calibrate_full_test (RW_MGR_MEM_DATA_WIDTH, &bit_chk, 1)) {
			break;
		}

		working_cnt++;
	}

	payload[0] = working_cnt;

	WRITE_SCC_DM_IO_OUT1_DELAY(dm, start);
	scc_mgr_load_dm (dm);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);
}


//USER precharge all banks and activate row 0 in bank "000..." and bank "111..." 
#if DDRX
void mem_precharge_and_activate (void)
{
	alt_u32 r;

	for (r = 0; r < RW_MGR_MEM_NUMBER_OF_RANKS; r++) {
		if (param->skip_ranks[r]) {
			//USER request to skip the rank

			continue;
		}

		set_rank_and_odt_mask(r, RW_MGR_ODT_MODE_OFF);

		//USER precharge all banks ... 
		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_PRECHARGE_ALL);

		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_0, 0, 0x0F);
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_0, 0, __RW_MGR_ACTIVATE_0_AND_1_WAIT1);

		IOWR_32DIRECT (RW_MGR_LOAD_CNTR_1, 0, 0x0F);
		IOWR_32DIRECT (RW_MGR_LOAD_JUMP_ADD_1, 0, __RW_MGR_ACTIVATE_0_AND_1_WAIT2);

		//USER activate rows 
		IOWR_32DIRECT (RW_MGR_RUN_SINGLE_GROUP, 0, __RW_MGR_ACTIVATE_0_AND_1);
	}
}
#endif

#if QDRII || RLDRAMII
void mem_precharge_and_activate (void) {}
#endif

//USER Configure various memory related parameters.
 
#if DDRX
void mem_config (void)
{
	alt_u32 rlat, wlat;
	alt_u32 rw_wl_nop_cycles;

	//USER read in write and read latency 

	wlat = IORD_32DIRECT (PHY_MGR_MEM_T_WL, 0);
	rlat = IORD_32DIRECT (PHY_MGR_MEM_T_RL, 0);

	if(HALF_RATE_MODE)
	{
		//USER In Half-Rate the WL-to-nop-cycles works like this
		//USER 0,1 -> -1
		//USER 2,3 -> 0
		//USER 4,5 -> 1
		//USER etc...
		if(wlat % 2)
		{
			rw_wl_nop_cycles = ((wlat - 1) / 2) - 1;
		}
		else
		{
			rw_wl_nop_cycles = (wlat / 2) - 1;
		}
	}
	else
	{
		rw_wl_nop_cycles = wlat - 2;
	}
	gbl->rw_wl_nop_cycles = rw_wl_nop_cycles;

	//USER configure for a burst length of 8

	if (HALF_RATE_MODE) {
		//USER write latency 

		wlat = (wlat / 2) + 1;

		//USER set a pretty high read latency initially 

		gbl->curr_read_lat = (rlat + 1) / 2 + 8;
		IOWR_32DIRECT (PHY_MGR_PHY_RLAT, 0, gbl->curr_read_lat);
	} else {
		//USER write latency 

		wlat = wlat + 1;

		//USER set a pretty high read latency initially 

		gbl->curr_read_lat = rlat + 16;
		IOWR_32DIRECT (PHY_MGR_PHY_RLAT, 0, gbl->curr_read_lat);
	}

	//USER advertise write latency 

	IOWR_32DIRECT (PHY_MGR_AFI_WLAT, 0, wlat - 1);

	mem_precharge_and_activate ();
}
#endif

#if QDRII || RLDRAMII
void mem_config (void)
{
	alt_u32 wlat, nop_cycles;
	if (HALF_RATE_MODE) {
		gbl->curr_read_lat = (IORD_32DIRECT (PHY_MGR_MEM_T_RL, 0) + 1) / 2 + 8;
	} else {
		gbl->curr_read_lat = IORD_32DIRECT (PHY_MGR_MEM_T_RL, 0) + 16;
	}
	IOWR_32DIRECT (PHY_MGR_PHY_RLAT, 0, gbl->curr_read_lat);
	
	if (RLDRAMII)
	{
		//USER read in write and read latency 
		wlat = IORD_32DIRECT (PHY_MGR_MEM_T_WL, 0);
		
		//USER In half rate mode, the NOP cycles look like this
		//USER 4/5 => 0
		//USER 6/7 => 1
		//USER 9 => 2
		if (HALF_RATE_MODE)
		{
			nop_cycles = (wlat / 2) - 2;
		}
		else
		{
			nop_cycles = wlat - 1;
		}
		gbl->rw_wl_nop_cycles = nop_cycles;
	}
}
#endif

//USER Set VFIFO and LFIFO to instant-on settings in skip calibration mode

void mem_skip_calibrate (void)
{
	alt_u32 vfifo_offset;
	alt_u32 i, v, j;

	//USER Set output phase alignment settings appropriate for skip calibration
	for (i = 0; i < RW_MGR_MEM_IF_READ_DQS_WIDTH; i++) {
#if IO_DLL_CHAIN_LENGTH == 6
		WRITE_SCC_DQS_EN_PHASE(i, (IO_DLL_CHAIN_LENGTH >> 1) - 1);
#else
		WRITE_SCC_DQS_EN_PHASE(i, (IO_DLL_CHAIN_LENGTH >> 1));
#endif
#if HCX_COMPAT_MODE && DDR3
		v = 0;
		for (j = 0; j < 2; j++) {
			rw_mgr_incr_vfifo(i, &v);
		}

		WRITE_SCC_DQDQS_OUT_PHASE(i, 6);
#else
		WRITE_SCC_DQDQS_OUT_PHASE(i, (IO_DLL_CHAIN_LENGTH - IO_DLL_CHAIN_LENGTH / 3));
#endif	
	}
	
	IOWR_32DIRECT (SCC_MGR_DQS_ENA, 0, 0xff);
	IOWR_32DIRECT (SCC_MGR_DQS_IO_ENA, 0, 0xff);
	IOWR_32DIRECT (SCC_MGR_UPD, 0, 0);


	for (i = 0; i < RW_MGR_MEM_IF_WRITE_DQS_WIDTH; i++) {
		IOWR_32DIRECT (SCC_MGR_GROUP_COUNTER, 0, i);
		IOWR_32DIRECT (SCC_MGR_DQ_ENA, 0, 0xff);
		IOWR_32DIRECT (SCC_MGR_DM_ENA, 0, 0xff);
	}

#if SUPPORT_DYNAMIC_SKIP_CALIBRATE_ACTIONS
	if ((DYNAMIC_CALIB_STEPS) & CALIB_IN_RTL_SIM) {
		//USER VFIFO is reset to the correct settings in RTL simulation 
	} else {
		vfifo_offset = IORD_32DIRECT (PHY_MGR_CALIB_VFIFO_OFFSET, 0);

		while (vfifo_offset > 1) {
			IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_HR, 0, 0xff);
			vfifo_offset -= 2;
		}

		if (vfifo_offset == 1) {
			IOWR_32DIRECT (PHY_MGR_CMD_INC_VFIFO_FR, 0, 0xff);
		}
	}
#endif

	IOWR_32DIRECT (PHY_MGR_CMD_FIFO_RESET, 0, 0);

	gbl->curr_read_lat = IORD_32DIRECT (PHY_MGR_CALIB_LFIFO_OFFSET, 0);
	IOWR_32DIRECT (PHY_MGR_PHY_RLAT, 0, gbl->curr_read_lat);
}

//USER Memory calibration entry point
 
alt_u32 mem_calibrate (void)
{
	alt_u32 i;
	alt_u32 write_group, write_test_bgn;
	alt_u32 read_group, read_test_bgn;
	alt_u32 run_groups, current_run;

	gbl->error_stage = CAL_STAGE_NIL;
	gbl->error_group = 0xff;
	gbl->fom_in = 0;
	gbl->fom_out = 0;

	if (((DYNAMIC_CALIB_STEPS) & CALIB_SKIP_ALL) == CALIB_SKIP_ALL) {
		//USER Set VFIFO and LFIFO to instant-on settings in skip calibration mode 

		mem_skip_calibrate ();
	} else {
		tcldbg_init_status_counters();

		mem_config ();

		for (i = 0; i < NUM_CALIB_REPEAT; i++) {
			//USER Zero all delay chain/phase settings 

			scc_mgr_zero_all ();

			run_groups = ~param->skip_groups;

			for (write_group = 0, write_test_bgn = 0; write_group < RW_MGR_MEM_IF_WRITE_DQS_WIDTH; write_group++, write_test_bgn += RW_MGR_MEM_DQ_PER_WRITE_DQS) {

				current_run = run_groups & ((1 << RW_MGR_NUM_DQS_PER_WRITE_GROUP) - 1);
				run_groups = run_groups >> RW_MGR_NUM_DQS_PER_WRITE_GROUP;

				if (current_run == 0)
				{
					continue;
				}

				IOWR_32DIRECT (SCC_MGR_GROUP_COUNTER, 0, write_group);
				scc_mgr_zero_group (write_group, write_test_bgn);

				for (read_group = write_group * RW_MGR_MEM_IF_READ_DQS_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH, read_test_bgn = 0;
				     read_group < (write_group + 1) * RW_MGR_MEM_IF_READ_DQS_WIDTH / RW_MGR_MEM_IF_WRITE_DQS_WIDTH;
				     read_group++, read_test_bgn += RW_MGR_MEM_DQ_PER_READ_DQS) {

					//USER Calibrate the VFIFO 
					if (!((STATIC_CALIB_STEPS) & CALIB_SKIP_VFIFO)) {
						if (!rw_mgr_mem_calibrate_vfifo (read_group, read_test_bgn)) {
							return 0;
						}
					}
				}

				//USER level writes (or align DK with CK for RLDRAMII) 
				if (DDRX || RLDRAMII)
				{
					if (!((STATIC_CALIB_STEPS) & CALIB_SKIP_WLEVEL)) {
						if (!rw_mgr_mem_calibrate_wlevel (write_group, write_test_bgn)) {
							return 0;
						}
					}
				}

				//USER Calibrate the output side 
				if (!((STATIC_CALIB_STEPS) & CALIB_SKIP_WRITES)) {
					if ((STATIC_CALIB_STEPS) & CALIB_SKIP_DELAY_SWEEPS) {
						//USER not needed in quick mode! 
					} else {
						if (!rw_mgr_mem_calibrate_writes (write_group, write_test_bgn)) {
							return 0;
						}
					}
				}
			}

			//USER Calibrate the LFIFO 
			if (!((STATIC_CALIB_STEPS) & CALIB_SKIP_LFIFO)) {
				//USER If we're skipping groups as part of debug, don't calibrate LFIFO
				if (param->skip_groups == 0)
				{
					if (!rw_mgr_mem_calibrate_lfifo ()) {
						return 0;
					}
				}
			}
		}
	}

	return 1;
}

alt_u32 run_mem_calibrate(void) {
	alt_u32 pass;
	alt_u32 debug_info;

	initialize();

	rw_mgr_mem_initialize ();

	pass = mem_calibrate ();

	mem_precharge_and_activate ();
	rw_mgr_mem_calibrate_read_load_patterns();

	//USER Handoff 

	//USER Don't return control of the PHY back to AFI when in debug mode
	if (!gbl->phy_in_debug_mode) {
		rw_mgr_mem_handoff ();

		IOWR_32DIRECT (PHY_MGR_MUX_SEL, 0, 0);
	}

	if (pass) {
		gbl->fom_in /= 2;
		gbl->fom_out /= 2;

		if (gbl->fom_in > 0xff) {
			gbl->fom_in = 0xff;
		}

		if (gbl->fom_out > 0xff) {
			gbl->fom_out = 0xff;
		}

		debug_info = gbl->fom_in;
		debug_info |= gbl->fom_out << 8;

		IOWR_32DIRECT (PHY_MGR_CAL_DEBUG_INFO, 0, debug_info);
		IOWR_32DIRECT (PHY_MGR_CAL_STATUS, 0, PHY_MGR_CAL_SUCCESS);
	} else {
		debug_info = gbl->error_stage;
		debug_info |= gbl->error_group << 8;

		IOWR_32DIRECT (PHY_MGR_CAL_DEBUG_INFO, 0, debug_info);
		IOWR_32DIRECT (PHY_MGR_CAL_STATUS, 0, PHY_MGR_CAL_FAIL);
	}

	return pass;

}

#if HCX_COMPAT_MODE
void hc_initialize_rom_data(void)
{
	alt_u32 i;

	for(i = 0; i < inst_rom_init_size; i++)
	{
		alt_u32 data = inst_rom_init[i];
		IOWR_32DIRECT (RW_MGR_INST_ROM_WRITE, (i << 2), data);
	}

	for(i = 0; i < ac_rom_init_size; i++)
	{
		alt_u32 data = ac_rom_init[i];
		IOWR_32DIRECT (RW_MGR_AC_ROM_WRITE, (i << 2), data);
	}
}
#endif


int main(void)
{
	param_t my_param;
	gbl_t my_gbl;
	tcldbg_rx_packet_t my_rx_packet;
	tcldbg_tx_packet_t my_tx_packet;

#if HCX_COMPAT_MODE	
	hc_initialize_rom_data();
#endif	

	IOWR_32DIRECT (RW_MGR_SOFT_RESET, 0, 0);

	//USER update info for sims
	IOWR_32DIRECT (PTR_MGR_INFO_STEP, 0, CAL_STAGE_NIL);
	IOWR_32DIRECT (PTR_MGR_INFO_GROUP, 0, 0);

	param = &my_param;
	gbl = &my_gbl;

	//USER Set the PHY as in user mode
	gbl->phy_in_debug_mode = 0;

	dyn_calib_steps = IORD_32DIRECT(PHY_MGR_CALIB_SKIP_STEPS, 0);

	if (!((DYNAMIC_CALIB_STEPS) & CALIB_SKIP_DELAY_LOOPS)) {
		skip_delay_mask = 0xff;
		skip_delay_val = 0x0;
	} else {
		skip_delay_mask = 0x0;
		skip_delay_val = 0x2;
	}

	//USER Initialize the TCL interface
	tcldbg_initialize (&my_rx_packet, &my_tx_packet);

	run_mem_calibrate ();

	tcl_debug_loop ();

	return 0;
}
