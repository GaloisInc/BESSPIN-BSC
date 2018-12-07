// Copyright 2010, Bluespec Inc, All Rights Reserved

#include "SceMiServiceThread.h"

// Static member for counting how many instantiations of thread
//  Error if more than one thread is instantiated
unsigned SceMiServiceThread::m_thread_count = 0;

