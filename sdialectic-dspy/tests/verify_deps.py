import sys

print(f"Python Executable: {sys.executable}")
print("Verifying dependencies...")

try:
    import fastapi
    print(f"[OK] fastapi {fastapi.__version__}")
except ImportError as e:
    print(f"[FAIL] fastapi: {e}")

try:
    import pydantic
    print(f"[OK] pydantic {pydantic.VERSION}")
except ImportError as e:
    print(f"[FAIL] pydantic: {e}")

try:
    import matplotlib
    print(f"[OK] matplotlib {matplotlib.__version__}")
except ImportError as e:
    print(f"[FAIL] matplotlib: {e}")

try:
    import pandas
    print(f"[OK] pandas {pandas.__version__}")
except ImportError as e:
    print(f"[FAIL] pandas: {e}")

try:
    import datasets
    print(f"[OK] datasets {datasets.__version__}")
except ImportError as e:
    print(f"[FAIL] datasets: {e}")

try:
    import bioc
    print(f"[OK] bioc (imported successfully)")
except ImportError as e:
    print(f"[FAIL] bioc: {e}")

try:
    import dspy
    print(f"[OK] dspy {dspy.__version__}")
except ImportError as e:
    print(f"[FAIL] dspy: {e}")

try:
    import google.genai
    print(f"[OK] google.genai (imported successfully)")
except ImportError as e:
    print(f"[FAIL] google.genai: {e}")

try:
    import uvicorn
    print(f"[OK] uvicorn {uvicorn.__version__}")
except ImportError as e:
    print(f"[FAIL] uvicorn: {e}")

print("Verification Complete.")
