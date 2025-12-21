import { create } from 'zustand';
import type { Group, Source } from '../types';


export interface Message {
  role: "user" | "model" | "tool";
  content: string;
}

interface DialecticState {
  groups: Group[];
  activeGroupId: string | null;
  activeSourceId: string | null;

  // Chat Persistence
  messages: Message[];

  // Graph State
  graphVersion: number;

  // Actions
  createGroup: (name: string) => void;
  setActiveGroup: (id: string) => void;
  addSourceToActiveGroup: (source: Source) => void;
  setActiveSource: (id: string | null) => void;

  addMessage: (msg: Message) => void;
  clearMessages: () => void;
  setMessages: (msgs: Message[]) => void;

  incrementGraphVersion: () => void;
}

export const useDialecticStore = create<DialecticState>((set, get) => ({
  groups: [],
  activeGroupId: null,
  activeSourceId: null,
  messages: [],
  graphVersion: 0,

  createGroup: (name: string) => {
    const newGroup: Group = {
      id: crypto.randomUUID(),
      name,
      sources: [],
    };
    set((state) => ({
      groups: [...state.groups, newGroup],
      activeGroupId: newGroup.id,
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
        g.id === activeGroupId ? { ...g, sources: [...g.sources, source] } : g
      ),
      activeSourceId: source.id,
    });
  },

  setActiveSource: (id: string | null) => {
    set({ activeSourceId: id });
  },

  addMessage: (msg: Message) =>
    set((state) => ({ messages: [...state.messages, msg] })),
  clearMessages: () => set({ messages: [] }),
  setMessages: (msgs: Message[]) => set({ messages: msgs }),

  incrementGraphVersion: () =>
    set((state) => ({ graphVersion: state.graphVersion + 1 })),
}));

