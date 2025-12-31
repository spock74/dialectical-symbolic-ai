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
  deleteGroup: (id: string) => void;
}

interface FilterState {
  showEntities: boolean;
  showRelations: boolean;
  selectedRelationTypes: string[];
}

interface GraphSlice {
  graphVersion: number;
  graphDirection: 'TB' | 'LR';
  sourceFilters: Record<string, FilterState>; // Per-source persistence
  
  // Actions
  incrementGraphVersion: () => void;
  setGraphDirection: (dir: 'TB' | 'LR') => void;
  
  // Flexible setters that implicitly use activeSourceId
  setShowEntities: (show: boolean) => void;
  setShowRelations: (show: boolean) => void;
  setSelectedRelationTypes: (types: string[]) => void;
  toggleRelationType: (type: string) => void;
  initializeSourceFilters: (sourceId: string, types: string[]) => void;
  
  // Selector Helpers
  getFilterState: (sourceId: string | null) => FilterState;
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
    // ... existing implementation ...
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

  deleteGroup: (id: string) => {
      set((state) => {
          const newGroups = state.groups.filter(g => g.id !== id);
          return {
              groups: newGroups,
              // If the deleted group was active, switch to null or the first available
              activeGroupId: state.activeGroupId === id ? null : state.activeGroupId
          };
      });
  },

  addSourceToActiveGroup: (source: Source) => {
    const { activeGroupId, groups } = get();
    // ... existing implementation ...
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

const createGraphSlice: StateCreator<CombinedState, [], [], GraphSlice> = (set, get) => ({
  graphVersion: 0,
  graphDirection: 'TB',
  sourceFilters: {},

  incrementGraphVersion: () =>
    set((state) => ({ graphVersion: state.graphVersion + 1 })),

  setGraphDirection: (dir) => set({ graphDirection: dir }),

  getFilterState: (sourceId) => {
    const { sourceFilters } = get();
    if (!sourceId || !sourceFilters[sourceId]) {
      return {
        showEntities: true,
        showRelations: true,
        selectedRelationTypes: [], // "Empty" implies "Not Initialized" or "None" depending on context, handled in UI
      };
    }
    return sourceFilters[sourceId];
  },

  initializeSourceFilters: (sourceId, allTypes) => set((state) => {
    // Only initialize if not already present to preserve user choices
    if (state.sourceFilters[sourceId]) return {};
    
    return {
      sourceFilters: {
        ...state.sourceFilters,
        [sourceId]: {
          showEntities: true,
          showRelations: true,
          selectedRelationTypes: allTypes, // Default to ALL selected
        }
      }
    };
  }),

  setShowEntities: (show) => set((state) => {
    const activeId = state.activeSourceId;
    if (!activeId) return {};

    const current = state.sourceFilters[activeId] || { showEntities: true, showRelations: true, selectedRelationTypes: [] };
    
    return {
      sourceFilters: {
        ...state.sourceFilters,
        [activeId]: { ...current, showEntities: show }
      }
    };
  }),

  setShowRelations: (show) => set((state) => {
    const activeId = state.activeSourceId;
    if (!activeId) return {};

    const current = state.sourceFilters[activeId] || { showEntities: true, showRelations: true, selectedRelationTypes: [] };

    return {
      sourceFilters: {
        ...state.sourceFilters,
        [activeId]: { ...current, showRelations: show }
      }
    };
  }),

  setSelectedRelationTypes: (types) => set((state) => {
    const activeId = state.activeSourceId;
    if (!activeId) return {};

    const current = state.sourceFilters[activeId] || { showEntities: true, showRelations: true, selectedRelationTypes: [] };

    return {
      sourceFilters: {
        ...state.sourceFilters,
        [activeId]: { ...current, selectedRelationTypes: types }
      }
    };
  }),

  toggleRelationType: (type) => set((state) => {
    const activeId = state.activeSourceId;
    if (!activeId) return {};

    const current = state.sourceFilters[activeId] || { showEntities: true, showRelations: true, selectedRelationTypes: [] };
    const currentTypes = current.selectedRelationTypes || [];
    
    const isSelected = currentTypes.includes(type);
    const newTypes = isSelected 
        ? currentTypes.filter(t => t !== type)
        : [...currentTypes, type];

    return {
      sourceFilters: {
        ...state.sourceFilters,
        [activeId]: { ...current, selectedRelationTypes: newTypes }
      }
    };
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
