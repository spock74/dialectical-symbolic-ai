import os
from dotenv import load_dotenv

# Load .env file from project root (one level up from core/)
# Assuming structure: sdialectic-dspy/.env
env_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '.env'))
load_dotenv(env_path)

class Config:
    # Service Names
    SERVICE_NAME = "SDialectic Core v2"
    VERSION = "2.0.0"
    
    # Model Configuration (Single Source of Truth)
    # User can override in .env
    TEACHER_MODEL_NAME = os.getenv("TEACHER_MODEL_NAME", "gemini/models/gemini-2.5-flash-lite")
    STUDENT_MODEL_NAME = os.getenv("STUDENT_MODEL_NAME", "ollama/phi4-mini:latest")
    
    # API Keys & Hosts
    GEMINI_API_KEY = os.getenv("GEMINI_API_KEY", "")
    OLLAMA_HOST = os.getenv("OLLAMA_HOST", "http://localhost:11434")
    
    # Paths
    DSPY_CACHE_DIR = os.getenv("DSPY_CACHE_DIR", "dspy_cache")

settings = Config()
