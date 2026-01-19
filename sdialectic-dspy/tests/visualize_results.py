import json
import matplotlib.pyplot as plt
import pandas as pd
import sys
import os

def generate_charts(json_path):
    print(f"[INFO] Generating charts from {json_path}...")
    
    with open(json_path, 'r') as f:
        data = json.load(f)
        
    rows = []
    for model_name, metrics in data.items():
        for res in metrics:
            rows.append({
                "Model": model_name,
                "Category": res['category'],
                "Valid": res['valid_syntax'] and res['valid_kernel'],
                "Hallucination": res.get('hallucination_found', False)
            })
            
    df = pd.DataFrame(rows)
    
    # 1. Bar Chart: Validity Rate by Category per Model
    if not df.empty:
        plt.figure(figsize=(12, 6))
        pivot = df.pivot_table(index='Category', columns='Model', values='Valid', aggfunc='mean')
        ax = pivot.plot(kind='bar', width=0.8)
        
        plt.title("Validity Rate (Syntax + Kernel) by Category")
        plt.ylabel("Pass Rate (0.0 - 1.0)")
        plt.xlabel("Category")
        plt.xticks(rotation=45)
        plt.legend(title="Model")
        plt.tight_layout()
        
        output_bar = os.path.join(os.path.dirname(json_path), "validity_chart.png")
        plt.savefig(output_bar)
        print(f"[INFO] Saved Validity Chart: {output_bar}")
        plt.close()

    # 2. Pie Chart: Hallucinations (Overall)
    hallucination_counts = df[df['Hallucination'] == True]['Model'].value_counts()
    
    if not hallucination_counts.empty:
        plt.figure(figsize=(6, 6))
        plt.pie(hallucination_counts, labels=hallucination_counts.index, autopct='%1.1f%%', startangle=140)
        plt.title("Share of Hallucinations Detected")
        
        output_pie = os.path.join(os.path.dirname(json_path), "hallucination_chart.png")
        plt.savefig(output_pie)
        print(f"[INFO] Saved Hallucination Chart: {output_pie}")
        plt.close()
    else:
        print("[INFO] No hallucinations detected! Skipping pie chart.")
        
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python visualize_results.py <results.json>")
        sys.exit(1)
        
    generate_charts(sys.argv[1])
