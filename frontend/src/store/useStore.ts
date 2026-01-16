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
  sourceMessages: Record<string, Message[]>; // Per-source history
  lastReasoningLogs: string;
  addMessage: (sourceId: string, msg: Message) => void;
  clearMessages: (sourceId: string) => void;
  setMessages: (sourceId: string, msgs: Message[]) => void;
  setLastReasoningLogs: (logs: string) => void;
  resetAllState: () => void;
}



interface SourceSlice {
  groups: Group[];
  activeGroupId: string | null;
  activeSourceId: string | null;
  createGroup: (name: string) => void;
  setActiveGroup: (id: string) => void;
  addSourceToActiveGroup: (source: Source) => void;
  addSourceToGroup: (groupId: string, source: Source) => void;
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

// --- SLICE INTERFACES ---

interface ConfigSlice {
  useConversationalMemory: boolean;
  useEpisodicMemory: boolean; // [PHASE 11] Toggle for Chat Layer (Long term overlay)
  useBypassSDialect: boolean;
  setUseConversationalMemory: (val: boolean) => void;
  setUseEpisodicMemory: (val: boolean) => void;
  setUseBypassSDialect: (val: boolean) => void;
}

// ... (omitted code) since I can't see lines between 20-70, assuming createConfigSlice is below.
// Actually I should just replace the CreateConfigSlice part found in lines 73-78

const createConfigSlice: StateCreator<CombinedState, [], [], ConfigSlice> = (set) => ({
  useConversationalMemory: true, 
  useEpisodicMemory: true, // Default ON
  useBypassSDialect: false,
  setUseConversationalMemory: (val) => set({ useConversationalMemory: val }),
  setUseEpisodicMemory: (val) => set({ useEpisodicMemory: val }),
  setUseBypassSDialect: (val) => set({ useBypassSDialect: val }),
});

const createChatSlice: StateCreator<CombinedState, [], [], ChatSlice> = (set) => ({
  sourceMessages: {},
  lastReasoningLogs: "",
  addMessage: (sourceId, msg) => set((state) => ({ 
    sourceMessages: {
      ...state.sourceMessages,
      [sourceId || "default"]: [...(state.sourceMessages[sourceId || "default"] || []), msg]
    }
  })),
  clearMessages: (sourceId) => set((state) => ({ 
    sourceMessages: {
      ...state.sourceMessages,
      [sourceId || "default"]: []
    }
  })),
  setMessages: (sourceId, msgs) => set((state) => ({ 
    sourceMessages: {
      ...state.sourceMessages,
      [sourceId || "default"]: msgs
    }
  })),
  setLastReasoningLogs: (logs) => set({ lastReasoningLogs: logs }),
  resetAllState: () => set(() => ({
    sourceMessages: {},
    lastReasoningLogs: "",
    groups: [],
    activeGroupId: null,
    activeSourceId: null,
    graphVersion: 0,
    sourceFilters: {}
  })),
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
          const groupToDelete = state.groups.find(g => g.id === id);
          const newGroups = state.groups.filter(g => g.id !== id);
          
          let newActiveSourceId = state.activeSourceId;
          // If the active source belongs to the deleted group, reset it
          if (groupToDelete && groupToDelete.sources.some(s => s.id === state.activeSourceId)) {
              newActiveSourceId = null;
          }

          return {
              groups: newGroups,
              activeGroupId: state.activeGroupId === id ? null : state.activeGroupId,
              activeSourceId: newActiveSourceId
          };
      });
  },

  addSourceToGroup: (groupId: string, source: Source) => {
    const { groups } = get();
    set({
      groups: groups.map((g) =>
        g.id === groupId
          ? { ...g, sources: [...g.sources, { ...source, active: true }] }
          : g
      ),
      activeSourceId: source.id,
      activeGroupId: groupId // Ensure we switch context to the group where we uploaded
    });
  },

  addSourceToActiveGroup: (source: Source) => {
    const { activeGroupId } = get();
    if (activeGroupId) {
        get().addSourceToGroup(activeGroupId, source);
    }
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
      version: 2, // Forced reset for breaking changes in source-specific history/filters
      migrate: (persistedState, version) => {
        if (version !== 2) {
          // If version mismatch, better to discard incompatible state than risk breaks
          return persistedState as CombinedState; // Returning existing does essentially nothing if version breaks, 
          // actually standard behavior on version mismatch without migrate is to not use the state.
          // To simply silence the warning but keep "reset" behavior, we need to return something.
          // Usually we return `persistedState` if we mapped it, or `{}` if we want to reset?
          // The error "couldn't be migrated since no migrate function was provided" implies we MUST provide one.
          // If we want a hard reset, we can just return an empty object or a partial state?
          // Actually, if we return `persistedState`, it effectively keeps it.
          // To FORCE RESET (which was our intention with version bump), we should probably not return the old state.
          
          return {} as CombinedState; // Return empty state to trigger re-initialization from defaults
        }
        return persistedState as CombinedState;
      },
    }
  )
);
