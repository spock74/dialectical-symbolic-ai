import { KnowledgeGraph } from './graph-engine';
// @ts-ignore
import * as jscl from 'jscl';

/**
 * JSCLAdapter: A bridge between the native Lisp engine (JSCL) 
 * and the TypeScript KnowledgeGraph.
 */
export class JSCLAdapter {
  private static instance: JSCLAdapter;
  private env: any;
  private currentGraph: KnowledgeGraph | null = null;

  private constructor() {
    this.initializeJSCL();
  }

  public static getInstance(): JSCLAdapter {
    if (!JSCLAdapter.instance) {
      JSCLAdapter.instance = new JSCLAdapter();
    }
    return JSCLAdapter.instance;
  }

  private initializeJSCL() {
    try {
      // @ts-ignore
      const engine = jscl.default || jscl;

      // Create a global interface for Lisp to call
      (global as any).TS_KERNEL = {
        addNode: (k: string, v: string) => this.currentGraph?.addNode(k, v),
        addRelation: (s: string, p: string, o: string) => this.currentGraph?.addRelation(s, p, o),
        infer: () => this.currentGraph?.infer(),
        doIntervention: (t: string, v: any) => this.currentGraph?.doIntervention(t, v),
        resetIntervention: () => this.currentGraph?.resetIntervention(),
        listNodes: () => this.currentGraph?.listNodes(),
        listRelations: () => this.currentGraph?.listRelations(),
        findRelations: (id: string) => this.currentGraph?.findRelations(id),
        getNode: (id: string) => this.currentGraph?.getNode(id)
      };



      // Define internal functions
      const fns = [
        '(defun adicionar-memoria (k v) (#j:TS_KERNEL:addNode k v))',
        '(defun adicionar-relacao (s p o) (#j:TS_KERNEL:addRelation s p o))',
        '(defun inferir () (#j:TS_KERNEL:infer))',
        '(defun intervention (target val) (#j:TS_KERNEL:doIntervention target val))',
        '(defun undo () (#j:TS_KERNEL:resetIntervention))',
        '(defun listar-memorias () (#j:TS_KERNEL:listNodes))',
        '(defun listar-relacoes () (#j:TS_KERNEL:listRelations))',
        '(defun buscar-relacoes (id) (#j:TS_KERNEL:findRelations id))',
        '(defun recuperar-memoria (id) (#j:TS_KERNEL:getNode id))',
        '(defun listar-regras () "() ; Nenhuma regra ativa no motor JSCL.")',
        // Alias do to intervention to avoid conflict with CL:DO
        '(defmacro do (target val) `(intervention ,target ,val))'
      ];

      for (const fn of fns) {
        try {
          engine.evaluateString(fn);
        } catch (err) {
          console.warn(`[JSCL-Adapter] Warning defining Lisp function: ${fn}`, err);
        }
      }
      
      console.log("[JSCL-Adapter] Common Lisp Environment Initialized with TS-Bridge.");
    } catch (e) {
      console.error("[JSCL-Adapter] Failed to initialize JSCL:", e);
    }
  }

  public eval(code: string, graph: KnowledgeGraph): any {
    this.currentGraph = graph;
    try {
      // @ts-ignore
      const engine = jscl.default || jscl;
      return engine.evaluateString(code);
    } catch (e: any) {
      throw new Error(`[Lisp Runtime Error]: ${e.message}`);
    }
  }
}

export const jsclAdapter = JSCLAdapter.getInstance();
