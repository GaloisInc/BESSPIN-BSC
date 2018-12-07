

void addInputChain(VeriModule* module, std::string *prefix, boost::regex filter, ScanPath* path, unsigned* includes_all)
{

  //Add ports scan_in, scan_out, scan_mode;

  char*      scan_width_name = (char*) getUnusedName(module, "_WIDTH");
  VeriIdDef* scan_width      = module->AddParameter(scan_width_name, 0 /* data type */, new VeriIntVal(1) /* initial value */) ;
  Globals::SetWidth(new VeriIdRef(scan_width));

  char*      scan_in_name   = (char*) getUnusedName(module, "_IN");
  VeriIdDef* scan_in        = addPort(module, scan_in_name, VERI_INPUT, Globals::GetWidth());
  printf("ADDING PORT %s TO %s\n", scan_in_name, module->GetName());

  char*      scan_out_name  = (char*) getUnusedName(module, "_OUT");
  VeriIdDef* scan_out       = addPort(module, scan_out_name, VERI_OUTPUT, Globals::GetWidth());

  char*      scan_mode_name = (char*) getUnusedName(module, "_MODE");
  VeriIdDef* scan_mode      = addPort(module, scan_mode_name, VERI_INPUT, 1);

  //  includes_all = 0;

  VeriIdDef* scan_internal  = scan_in;

  
  unsigned        before    = scannedBefore(module);
  VeriExpression* scan_cond = createScanCondition(module, ref(scan_mode));

  // The two expressions above must come before this one (since getAnyScan updates the anyScan value).
  VeriIdDef*      any_scan  = getAnyScan(module, ref(scan_mode));

  unsigned includes_none = 1;
  VeriEventControlStatement *stmt;
  foreachAlways(module, stmt) {
    if (isClocked(stmt)) {
      VeriStatement* body_orig = stmt->GetStmt() ;
      VeriStatement* body = Copy(body_orig);
      VeriStatement* block;

      if (before) {
	block = body; // This is a VeriSeqBlock
	body  = getScanBlockBody(block);
      } else {
	VeriExpression* cond_run           = new VeriUnaryOperator(VERI_LOGNOT, new VeriIdRef(any_scan));
	VeriConditionalStatement* run_stmt = new VeriConditionalStatement(cond_run, body, NULL) ;
	Array *stmts = new Array();
	stmts->Insert(run_stmt);
	block = new VeriSeqBlock(0,0,stmts,0);
	block->Resolve(module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV);
	//	stmt->ReplaceChildStmt(body_orig, block);
      }

      VeriStatement* scan_block = createScanBlock(module, body, &scan_internal, prefix, filter, path, includes_all, includes_none);

      if (scan_block) {
	markScanClocks(module, stmt);
	VeriConditionalStatement* scan_stmt = new VeriConditionalStatement(scan_cond, scan_block, NULL);
	scan_stmt->Resolve(module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV) ;
	VeriSeqBlock* block_new = addStatement((VeriSeqBlock*) block, scan_stmt);
	stmt->ReplaceChildStmt(body_orig, block_new);
	includes_none = 0;
      }
    }
  }

  connectScanInstances(module, &scan_internal, scan_mode, path);

  connectScanOuput(module, scan_internal, scan_out);

  char*      scan_count_name  = (char*) getUnusedName(module, "_COUNT");
  VeriIdDef* scan_count       = addPort(module, scan_count_name, VERI_OUTPUT, 32);

  connectScanCount(module, path, scan_width, scan_count);

  addAttribute(module, "SCAN_IN",    scan_in_name);
  addAttribute(module, "SCAN_OUT",   scan_out_name);
  addAttribute(module, "SCAN_MODE",  scan_mode_name);
  addAttribute(module, "SCAN_COUNT", scan_count_name);

}
