# Análise Estatística do Desempenho Escolar 

Este repositório contém a análise estatística desenvolvida a partir de uma base de dados real de alunos de duas escolas portuguesas: **Gabriel Pereira** e **Mousinho da Silva**. O estudo buscou identificar fatores associados ao desempenho acadêmico, com foco especial na **nota final de Português (G3)**.

---

##  Objetivo

Investigar padrões e relações entre fatores socioeconômicos, familiares, educacionais e comportamentais que possam influenciar o desempenho dos alunos, utilizando técnicas estatísticas e implementações em **linguagem R**.

---

##  Metodologia

* Foram selecionadas **16 variáveis** de interesse dentre as 33 disponíveis na base.
* As variáveis foram agrupadas em 5 grandes eixos temáticos:

  * Educação
  * Família
  * Situação Econômica / Região
  * Vida Social / Saúde
  * Preferências e hábitos pessoais
* As análises foram feitas utilizando o **RStudio**, com geração de gráficos, tabelas descritivas e cálculo do **grau de associação (R²)** entre cada variável e a nota final.
* Os dados originais foram mantidos sem modificação.

---

##  Estrutura do Repositório

* `script_analise.R` — script principal com toda a análise estatística (em R)
* `Relatório de Estatística-Grupo 9.pdf` — relatório final com visualizações, tabelas e conclusões
* `base_dados.csv` — conjunto de dados utilizado na análise (caso disponibilizado)

---

##  Principais Resultados

* **Fatores com maior associação à nota final:**

  * Preferência por cursar o Ensino Superior (R² = 10,9%)
  * Escola frequentada (R² = 7,89%)
  * Tempo dedicado ao estudo (R² = 6,41%)
  * Consumo diário de álcool (R² = 4,14%)
* **Fatores com associação moderada:**

  * Localidade (urbano/rural), acesso à internet e tempo de deslocamento até a escola
* **Fatores com associação fraca:**

  * Apoio escolar e familiar, vida social, estado de saúde

>  As associações não indicam causalidade, apenas correlações observacionais.

---

##  Ferramentas Utilizadas

* **R** e **RStudio**
* Pacotes:

  * `tidyverse`
  * `ggplot2`
  * `dplyr`
  * `readr`
  * `knitr` (para visualizações no relatório)

---

##  Integrantes do Grupo

* Davi Lucas de Jesus Caetano
* Juliana Roncaglione Bevilaqcua Câmara
* Isabella Rodrigues de Paulo
* Paolla Pinheiro Pacheco

---

## Observações

* A base de dados é de domínio público e foi adaptada para fins educacionais.
* O projeto foi desenvolvido como parte das atividades da disciplina de  Introdução a Estatística (UFF, 2024).
* Para reproduzir os resultados, basta rodar o script `script_analise.R` com a base fornecida.

---
