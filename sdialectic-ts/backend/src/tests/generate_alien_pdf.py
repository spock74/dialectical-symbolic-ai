from fpdf import FPDF

class PDF(FPDF):
    def header(self):
        # Cabeçalho simulando um jornal científico sério
        self.set_font('Arial', 'B', 15)
        self.cell(0, 10, 'Journal of Exobiology - Vol 42', 0, 1, 'C')
        self.ln(10)

pdf = PDF()
pdf.add_page()
pdf.set_font("Arial", size=12)

# O texto foi desenhado com palavras-chave fortes ("fundamental law", "strictly prohibited")
# para acionar o seu prompt de extração de Leis (adicionar-regra).
text = """
Title: Toxicity Analysis of Xylophonia Fruits in Human Metabolism

Abstract:
This study analyzes the chemical composition of the Xylophonia fruit, native to sector 7G. While possessing high caloric density, our spectral analysis confirms the presence of Zeta-Toxin in high concentrations (40mg/g).

Biological Law 1: Zeta-Toxin Interaction
It is a fundamental biological law that Zeta-Toxin causes immediate cellular apoptosis in human tissue.

Safety Regulation:
Due to the presence of Zeta-Toxin, the consumption of Xylophonia is strictly prohibited. Any agent containing Zeta-Toxin is classified as a CLASS-A POISON.

Conclusion:
Despite its pleasant aroma, Xylophonia represents a fatal risk to humans and must never be ingested.
"""

# Escreve o corpo do texto
pdf.multi_cell(0, 10, text)

# Salva o arquivo
filename = "alien_biology.pdf"
pdf.output(filename)

print(f"Sucesso: O arquivo '{filename}' foi gerado.")
print("Passo seguinte: Faça upload deste PDF no SDialectic e tente pedir para comer a fruta.")

