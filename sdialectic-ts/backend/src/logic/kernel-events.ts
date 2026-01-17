/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

import { EventEmitter } from 'events';

class KernelEvents extends EventEmitter {
  private static instance: KernelEvents;

  private constructor() {
    super();
  }

  static getInstance(): KernelEvents {
    if (!KernelEvents.instance) {
      KernelEvents.instance = new KernelEvents();
    }
    return KernelEvents.instance;
  }
}

export const kernelEvents = KernelEvents.getInstance();
