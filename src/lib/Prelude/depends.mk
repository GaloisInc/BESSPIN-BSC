## Automatically generated by bluetcl -exec makedepend -- Do NOT EDIT
## Date: Wed Jun 19 15:10:48 EDT 2013
## Command: bluetcl -exec makedepend -bdir $(BDIR) *.bs*

$(BDIR)/ActionSeq.bo:	ActionSeq.bs $(BDIR)/List.bo $(BDIR)/Vector.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Assert.bo:	Assert.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Clocks.bo:	Clocks.bsv $(BDIR)/List.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/ConfigReg.bo:	ConfigReg.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Connectable.bo:	Connectable.bs $(BDIR)/Vector.bo $(BDIR)/ListN.bo $(BDIR)/Inout.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Counter.bo:	Counter.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Enum.bo:	Enum.bs $(BDIR)/List.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Environment.bo:	Environment.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/FIFO.bo:	FIFO.bs $(BDIR)/FIFOF_.bo $(BDIR)/FIFOF.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/FIFOF.bo:	FIFOF.bs $(BDIR)/FIFOF_.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/FIFOF_.bo:	FIFOF_.bsv $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/FIFOLevel.bo:	FIFOLevel.bsv $(BDIR)/FIFOF_.bo $(BDIR)/GetPut.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/FShow.bo:	FShow.bsv $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/FoldFIFO.bo:	FoldFIFO.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/FoldFIFOF.bo:	FoldFIFOF.bs $(BDIR)/FIFOF.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Fork.bo:	Fork.bs $(BDIR)/Vector.bo $(BDIR)/List.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/GetPut.bo:	GetPut.bs $(BDIR)/FIFO.bo $(BDIR)/FIFOF.bo $(BDIR)/Connectable.bo $(BDIR)/Clocks.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Inout.bo:	Inout.bsv $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/LevelFIFO.bo:	LevelFIFO.bsv $(BDIR)/FIFOLevel.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/List.bo:	List.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/ListN.bo:	ListN.bs $(BDIR)/List.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Once.bo:	Once.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Prelude.bo:	Prelude.bs
$(BDIR)/PreludeBSV.bo:	PreludeBSV.bsv $(BDIR)/Prelude.bo $(BDIR)/Prelude.bo
$(BDIR)/Array.bo:	Array.bsv $(BDIR)/List.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Probe.bo:	Probe.bsv $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/ProbeWire.bo:	ProbeWire.bsv $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/RWire.bo:	RWire.bsv $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Real.bo:	Real.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/RegFile.bo:	RegFile.bs $(BDIR)/ConfigReg.bo $(BDIR)/List.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Reserved.bo:	Reserved.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/RevertingVirtualReg.bo:	RevertingVirtualReg.bs $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
$(BDIR)/Vector.bo:	Vector.bs $(BDIR)/List.bo $(BDIR)/Array.bo $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo
