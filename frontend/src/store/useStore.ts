import { create, type StateCreator } from 'zustand';
import { persist } from "zustand/middleware";
import type { Group, Source } from "../types";

export interface Message {
  role: "user" | "model" | "tool";
  content: string;
}

// --- SLICE DEFINITIONS ---

interface ConfigSlice {
  useConversationalMemory: boolean;
  useBypassSDialect: boolean;
  setUseConversationalMemory: (val: boolean) => void;
  setUseBypassSDialect: (val: boolean) => void;
}

interface ChatSlice {
  messages: Message[];
  lastReasoningLogs: string;
  addMessage: (msg: Message) => void;
  clearMessages: () => void;
  setMessages: (msgs: Message[]) => void;
  setLastReasoningLogs: (logs: string) => void;
}

interface SourceSlice {
  groups: Group[];
  activeGroupId: string | null;
  activeSourceId: string | null;
  createGroup: (name: string) => void;
  setActiveGroup: (id: string) => void;
  addSourceToActiveGroup: (source: Source) => void;
  setActiveSource: (id: string | null) => void;
}

interface GraphSlice {
  graphVersion: number;
  showEntities: boolean;
  showRelations: boolean;
  graphDirection: 'TB' | 'LR';
  selectedRelationTypes: string[];
  incrementGraphVersion: () => void;
  setShowEntities: (show: boolean) => void;
  setShowRelations: (show: boolean) => void;
  setGraphDirection: (dir: 'TB' | 'LR') => void;
  setSelectedRelationTypes: (types: string[]) => void;
  toggleRelationType: (type: string) => void;
}

type CombinedState = ConfigSlice & ChatSlice & SourceSlice & GraphSlice;

// --- SLICE IMPLEMENTATIONS ---

const createConfigSlice: StateCreator<CombinedState, [], [], ConfigSlice> = (set) => ({
  useConversationalMemory: true,
  useBypassSDialect: false,
  setUseConversationalMemory: (val) => set({ useConversationalMemory: val }),
  setUseBypassSDialect: (val) => set({ useBypassSDialect: val }),
});

const createChatSlice: StateCreator<CombinedState, [], [], ChatSlice> = (set) => ({
  messages: [],
  lastReasoningLogs: "",
  addMessage: (msg) => set((state) => ({ messages: [...state.messages, msg] })),
  clearMessages: () => set({ messages: [] }),
  setMessages: (msgs) => set({ messages: msgs }),
  setLastReasoningLogs: (logs) => set({ lastReasoningLogs: logs }),
});

const createSourceSlice: StateCreator<CombinedState, [], [], SourceSlice> = (set, get) => ({
  groups: [],
  activeGroupId: null,
  activeSourceId: null,

  createGroup: (name: string) => {
    const now = new Date();
    const formattedDate = now.toLocaleString("pt-BR", {
      day: "2-digit",
      month: "2-digit",
      year: "numeric",
      hour: "2-digit",
      minute: "2-digit",
    });

    const newGroup: Group = {
      id: crypto.randomUUID(),
      name,
      createdAt: formattedDate,
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
        g.id === activeGroupId
          ? { ...g, sources: [...g.sources, { ...source, active: true }] }
          : g
      ),
      activeSourceId: source.id,
    });
  },

  setActiveSource: (id: string | null) => {
    set({ activeSourceId: id });
  },
});

const createGraphSlice: StateCreator<CombinedState, [], [], GraphSlice> = (set) => ({
  graphVersion: 0,
  showEntities: true,
  showRelations: true,
  graphDirection: 'TB',
  selectedRelationTypes: [], // Empty means all

  incrementGraphVersion: () =>
    set((state) => ({ graphVersion: state.graphVersion + 1 })),

  setShowEntities: (show) => set({ showEntities: show }),
  setShowRelations: (show) => set({ showRelations: show }),
  setGraphDirection: (dir) => set({ graphDirection: dir }),
  setSelectedRelationTypes: (types) => set({ selectedRelationTypes: types }),
  toggleRelationType: (type: string) => set((state) => {
    const isSelected = state.selectedRelationTypes.includes(type);
    const newTypes = isSelected 
        ? state.selectedRelationTypes.filter(t => t !== type)
        : [...state.selectedRelationTypes, type];
    return { selectedRelationTypes: newTypes };
  }),
});

// --- MAIN STORE ---

export const useDialecticStore = create<CombinedState>()(
  persist(
    (...a) => ({
      ...createConfigSlice(...a),
      ...createChatSlice(...a),
      ...createSourceSlice(...a),
      ...createGraphSlice(...a),
    }),
    {
      name: "dialectic-storage",
    }
  )
);
