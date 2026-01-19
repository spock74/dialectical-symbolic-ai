import os
import dspy
from google import genai
from google.genai import types
from dspy.dsp.utils import settings

class NativeGeminiClient(dspy.BaseLM):
    """
    A custom DSPy LM client that uses the official Google GenAI SDK (v1.0+)
    to interact with Gemini models, bypassing LiteLLM.
    """
    
    def __init__(
        self,
        model: str,
        api_key: str | None = None,
        temperature: float = 0.0,
        max_tokens: int = 8192,
        **kwargs
    ):
        """
        Args:
            model: The official model name (e.g., "gemini-2.5-flash-lite").
            api_key: Google GenAI API Key.
        """
        super().__init__(model=model, model_type="chat", temperature=temperature, max_tokens=max_tokens, **kwargs)
        
        self.api_key = api_key or os.environ.get("GEMINI_API_KEY")
        if not self.api_key:
            raise ValueError("GEMINI_API_KEY is not set.")
            
        self.client = genai.Client(api_key=self.api_key)
        self.provider = "google"

    def forward(
        self,
        prompt: str | None = None,
        messages: list[dict[str, Any]] | None = None,
        **kwargs
    ):
        """
        Sends a request to the Gemini API and parses the response into 
        the format DSPy expects (list of strings).
        """
        # 1. Merge Configuration
        config = {**self.kwargs, **kwargs}
        temperature = config.get("temperature", 0.0)
        max_tokens = config.get("max_tokens", 8192)
        
        # 2. Build Messages
        # valid_messages = []
        # if messages:
        #     for m in messages:
        #         role = m.get("role", "user")
        #         content = m.get("content", "")
        #         valid_messages.append(types.Content(role=role, parts=[types.Part.from_text(text=content)]))
        # elif prompt:
        #     valid_messages.append(types.Content(role="user", parts=[types.Part.from_text(text=prompt)]))

        # SIMPLIFICATION: The google-genai SDK 1.0+ supports list of dicts directly in many cases,
        # or we construct the string prompt for raw generation if needed.
        # However, DSPy usually passes 'messages' for Chat LMs.
        
        # We need to adapt OpenAI-style messages to Gemini Content objects if we use strict typing,
        # OR rely on the high-level generate_content helper which is flexible.
        
        # Let's construct a simple prompt string if possible, or correct contents.
        # Gemini 2.5 Flash Lite supports standard chat history.
        
        contents = []
        system_instruction = None
        
        if messages:
            for m in messages:
                role = m.get("role", "user")
                text = m.get("content", "")
                
                if role == "system":
                    system_instruction = text
                elif role == "user":
                    contents.append(types.Content(role="user", parts=[types.Part.from_text(text=text)]))
                elif role == "assistant":
                    contents.append(types.Content(role="model", parts=[types.Part.from_text(text=text)]))
        elif prompt:
             contents.append(types.Content(role="user", parts=[types.Part.from_text(text=prompt)]))

        # 3. Call API
        try:
            # Generate Content
            response = self.client.models.generate_content(
                model=self.model,
                contents=contents,
                config=types.GenerateContentConfig(
                    temperature=temperature,
                    max_output_tokens=max_tokens,
                    system_instruction=system_instruction
                )
            )
            
            # 4. Extract Text
            generated_text = response.text
            
            # 5. Usage Tracking (Manual, since we bypass litellm)
            # We construct a pseudo-response object expected by BaseLM._process_lm_response if we used it,
            # but BaseLM._process_lm_response is tightly coupled to OpenAI format.
            # So we will just return the list of strings directly as per BaseLM contract,
            # AND manually handle history/usage if we want to be nice citizens.
            
            # Update history manually to be safe
            # 'response' in history should be a dict containing 'usage' for GeminiLogger to pick it up
            usage_data = {
                "prompt_tokens": response.usage_metadata.prompt_token_count if response.usage_metadata else 0,
                "completion_tokens": response.usage_metadata.candidates_token_count if response.usage_metadata else 0,
                "total_tokens": response.usage_metadata.total_token_count if response.usage_metadata else 0
            }
            
            self.history.append({
                "prompt": prompt,
                "messages": messages,
                "kwargs": config,
                "response": { # GeminiLogger expects this to be a dict
                    "usage": usage_data,
                    "choices": [{"message": {"content": generated_text}}]
                }, 
                "outputs": [generated_text],
                "usage": usage_data, # Redundant but good for other inspectors
                "model": self.model
            })
            
            return [generated_text]
            
        except Exception as e:
            print(f"[NativeGeminiClient] Error: {e}")
            raise e

    def dump_state(self):
        return {
            "model": self.model,
            "provider": self.provider,
            "kwargs": self.kwargs
        }
