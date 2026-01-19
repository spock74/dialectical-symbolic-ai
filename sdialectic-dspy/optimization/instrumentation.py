import time
import csv
import os
import datetime
import dspy

class GeminiLogger:
    def __init__(self, lm, filename="gemini_usage_log.csv"):
        self.lm = lm
        self.filename = os.path.join(os.getcwd(), filename)
        
        # Init CSV header if not exists
        if not os.path.exists(self.filename):
            with open(self.filename, 'w', newline='', encoding='utf-8') as f:
                writer = csv.writer(f)
                writer.writerow(["Timestamp", "Status", "Model", "Input_Tokens", "Output_Tokens", "Total_Tokens", "Latency_Sec", "Error_Msg"])
        
        print(f"[INFO] Gemini Logger initialized: {self.filename}")

    def __call__(self, *args, **kwargs):
        start_time = time.time()
        success = False
        error_msg = ""
        usage = {"prompt_tokens": 0, "completion_tokens": 0, "total_tokens": 0}
        model_name = getattr(self.lm, 'model', 'unknown')
        
        try:
            # Execute actual call
            response = self.lm(*args, **kwargs)
            success = True
            
            # Attempt to capture usage from history
            # dspy.LM stores history in self.lm.history
            if hasattr(self.lm, 'history') and self.lm.history:
                last_interaction = self.lm.history[-1]
                raw_response = last_interaction.get('response', {})
                
                # OPTION 1: Standard LiteLLM/OpenAI format
                if 'usage' in raw_response:
                     u = raw_response['usage']
                     usage['prompt_tokens'] = u.get('prompt_tokens', 0)
                     usage['completion_tokens'] = u.get('completion_tokens', 0)
                     usage['total_tokens'] = u.get('total_tokens', 0)
                     
                # OPTION 2: Google GenAI Native (usage_metadata)
                # It might be nested or direct depending on the unexpected structure
                elif 'usage_metadata' in raw_response:
                    u = raw_response['usage_metadata']
                    # Google keys might be prompt_token_count etc.
                    usage['prompt_tokens'] = u.get('prompt_token_count', 0)
                    usage['completion_tokens'] = u.get('candidates_token_count', u.get('total_token_count', 0) - u.get('prompt_token_count', 0))
                    usage['total_tokens'] = u.get('total_token_count', 0)
                    
                # OPTION 3: Fallback Layouts (debug print if needed)
                else:
                    # Check for 'metadata' field which sometimes holds it
                    meta = raw_response.get('metadata', {})
                    if 'tokenUsage' in meta:
                        usage = meta['tokenUsage'] # If matched format
                    elif 'usage' in meta:
                        u = meta['usage']
                        usage['prompt_tokens'] = u.get('prompt_tokens', 0)
                        usage['completion_tokens'] = u.get('completion_tokens', 0)
                        usage['total_tokens'] = u.get('total_tokens', 0)
                
            return response

        except Exception as e:
            error_msg = str(e)
            # Re-raise to ensure program flow halts on error if necessary,
            # but for logging we might want to just record valid failures.
            # However, if it's a critical error like NotFound, we should raise.
            raise e
        
        finally:
            end_time = time.time()
            latency = end_time - start_time
            timestamp = datetime.datetime.now().isoformat()
            
            # Log to CSV
            try:
                with open(self.filename, 'a', newline='', encoding='utf-8') as f:
                    writer = csv.writer(f)
                    writer.writerow([
                        timestamp,
                        "SUCCESS" if success else "FAIL",
                        model_name,
                        usage['prompt_tokens'],
                        usage['completion_tokens'],
                        usage['total_tokens'],
                        f"{latency:.4f}",
                        error_msg
                    ])
            except Exception as log_err:
                print(f"[WARN] Logging failed: {log_err}")

    def __getattr__(self, name):
        # Proxy other attributes to the underlying LM
        return getattr(self.lm, name)
