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
