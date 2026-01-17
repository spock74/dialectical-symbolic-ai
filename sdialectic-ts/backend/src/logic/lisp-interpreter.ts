/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

import { KnowledgeGraph } from './graph-engine';
import { SBCLProcess } from '../services/sbcl-process';
import { kernelEvents } from './kernel-events';

export class LispInterpreter {
  execute(input: string, graph: KnowledgeGraph): string {
    // Envia direto para o processo SBCL
    // Nota: O método evaluate agora é assíncrono, o Orchestrator deve tratar await
    // Mas para manter compatibilidade síncrona se necessário, dispare e esqueça ou refatore o Orchestrator.
    // IDEAL: Refatorar Orchestrator para await lispInterpreter.execute()
    
    // Dispara a execução assíncrona mas retorna mensagem placeholder para compatibilidade imediata
    this.executeAsync(input).then(res => {
        kernelEvents.emit('log', `[SBCL Async Result]: ${res}`);
    }).catch(err => {
        kernelEvents.emit('log', `[SBCL Async Error]: ${err}`);
    });

    return "Comando enviado ao SBCL (Async Check logs)"; 
  }

  // Método real async que o Orchestrator deve usar
  async executeAsync(input: string): Promise<string> {
     return await SBCLProcess.getInstance().evaluate(input);
  }
}
export const lispInterpreter = new LispInterpreter();
