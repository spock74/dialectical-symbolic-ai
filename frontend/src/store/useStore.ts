import { create } from 'zustand';
import type { Group, Source } from '../types';

interface DialecticState {
  groups: Group[];
  activeGroupId: string | null;
  activeSourceId: string | null;

  // Actions
  createGroup: (name: string) => void;
  setActiveGroup: (id: string) => void;
  addSourceToActiveGroup: (source: Source) => void;
  setActiveSource: (id: string | null) => void; // Null allows deselecting
}

export interface Message {
  role: 'user' | 'model' | 'tool';
  content: string;
}

interface DialecticState {
  groups: Group[];
  activeGroupId: string | null;
  activeSourceId: string | null;
  
  // Chat Persistence
  messages: Message[];

  // Actions
  createGroup: (name: string) => void;
  setActiveGroup: (id: string) => void;
  addSourceToActiveGroup: (source: Source) => void;
  setActiveSource: (id: string | null) => void; // Null allows deselecting
  
  addMessage: (msg: Message) => void;
  clearMessages: () => void;
  setMessages: (msgs: Message[]) => void;
}

export const useDialecticStore = create<DialecticState>((set, get) => ({
  groups: [],
  activeGroupId: null,
  activeSourceId: null,
  messages: [],

  createGroup: (name: string) => {
    const newGroup: Group = {
      id: crypto.randomUUID(),
      name,
      sources: []
    };
    set((state) => ({
      groups: [...state.groups, newGroup],
      activeGroupId: newGroup.id // Auto-select new group
    }));
  },

  setActiveGroup: (id: string) => {
    set({ activeGroupId: id });
  },

  addSourceToActiveGroup: (source: Source) => {
    const { activeGroupId, groups } = get();
    if (!activeGroupId) return;

    set({
      groups: groups.map((g) => 
        g.id === activeGroupId 
          ? { ...g, sources: [...g.sources, source] }
          : g
      ),
      activeSourceId: source.id // Auto-select uploaded source
    });
  },

  setActiveSource: (id: string | null) => {
    // VITE_ALLOW_MULTIPLE_SOURCES logic check (implied false by default for now)
    // Since we store a single string ID, multiple selection is naturally prevented here.
    // If we wanted multiple, we'd use string[] and toggle logic.
    set({ activeSourceId: id });
  },

  addMessage: (msg: Message) => set((state) => ({ messages: [...state.messages, msg] })),
  clearMessages: () => set({ messages: [] }),
  setMessages: (msgs: Message[]) => set({ messages: msgs })
}));
