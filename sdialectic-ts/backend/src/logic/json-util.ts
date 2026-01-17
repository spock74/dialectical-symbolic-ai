/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

/**
 * Sanitizes a string that may contain JSON, potentially wrapped in markdown blocks 
 * or containing preamble/postamble.
 */
export function sanitizeJSON(text: string): string {
  if (!text) return "";
  
  let cleaned = text.trim();
  
  // 1. Remove Markdown code blocks if present
  const markdownRegex = /```(?:json)?\s*([\s\S]*?)\s*```/i;
  const match = cleaned.match(markdownRegex);
  if (match && match[1]) {
    cleaned = match[1].trim();
  }
  
  // 2. If it's still not starting with [ or {, try to find the first occurrence
  if (!cleaned.startsWith('{') && !cleaned.startsWith('[')) {
    const firstBrace = cleaned.indexOf('{');
    const firstBracket = cleaned.indexOf('[');
    
    let startIdx = -1;
    if (firstBrace !== -1 && firstBracket !== -1) {
      startIdx = Math.min(firstBrace, firstBracket);
    } else {
      startIdx = firstBrace !== -1 ? firstBrace : firstBracket;
    }
    
    if (startIdx !== -1) {
      cleaned = cleaned.substring(startIdx);
    }
    
    // Similarly for the end
    const lastBrace = cleaned.lastIndexOf('}');
    const lastBracket = cleaned.lastIndexOf(']');
    
    let endIdx = -1;
    if (lastBrace !== -1 && lastBracket !== -1) {
      endIdx = Math.max(lastBrace, lastBracket);
    } else {
      endIdx = lastBrace !== -1 ? lastBrace : lastBracket;
    }
    
    if (endIdx !== -1) {
      cleaned = cleaned.substring(0, endIdx + 1);
    }
  }
  
  return cleaned;
}

/**
 * Safely parses JSON from a potentially messy string.
 */
export function safeParseJSON<T>(text: string): T | null {
  const sanitized = sanitizeJSON(text);
  try {
    return JSON.parse(sanitized) as T;
  } catch (e) {
    console.error("[JSON-Util] Failed to parse sanitized JSON:", e);
    console.debug("[JSON-Util] Sanitized text was:", sanitized);
    return null;
  }
}
