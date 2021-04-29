<h1 align="center">Projeto: Unidade de Terapia Intensiva (Intensive Care Unit - ICU)</h1>

Este repositório contém análise exploratória, criação de modelo supervisionado de machine learning e de um relatório em Rmarkdown para dados de uma Unidade de Terapia Intensiva.

<h1 align="center">
  <img src="https://github.com/leonvictorlima/Intensive_Care_Unit_Analysis/blob/main/images/ICU.png"  width="800"/>
</h1>

<a name="introdução"></a>

# Modelo Supervisionado de Classificação 

## Requerimentos para o projeto

O projeto foi desenvolvido em linguagem R versão: 3.6.3 (2020-02-29). Para utilizar todos os scripts deste projeto, é necessário as seguintes bibliotecas de R: 

## Bibliotecas R
=================
<!--ts-->
   * [dplyr](https://dplyr.tidyverse.org/)
   * [ggplot2](https://ggplot2.tidyverse.org/)
   * [plotly](https://plotly.com/)
   * [webshot](https://github.com/wch/webshot)
   * [lattice](https://www.statmethods.net/advgraphs/trellis.html)
   * [caret](https://cran.r-project.org/web/packages/caret/caret.pdf)
   * [grid](https://www.rdocumentation.org/packages/grid/versions/3.6.2)
   * [tools_package]()
<!--te-->

## Sumário do Projeto

Este projeto é de identificação e análise de uma unidade intensiva de tratamento. Os pontos principais são: efetuar análise exploratória dos dados utilizando gráficos para gerar insights, efetuar a criação do modelo de machine learning capaz de prever a morte de um paciente desta unidade e gerar um relatório em Rmarkdown contendo os resultados obtidos.

Além disso, como componente fundamental nesta abordagem, a criação de uma biblioteca em R contendo todas as funções utilizadas para criação do respectivo relatório.

Os dados estão disponibilizados neste repositório em um arquivo .R e contém cerca de 10.000 (dez mil) linhas para execução destas tarefas.

Caso não possua o R instalado, acesse o site do [RStudio](https://www.rstudio.com/) e efetue o download.

## Código

Nesta análise os códigos foram divididos pensando em facilitar o entendimento, e com o objetivo de exemplificar diversas abordagens para efetuar a mesma tarefa. Portanto, abaixo estão listados os arquivos e suas descrições:

1. Arquivo PDF contendo a descrição das tarefas para o projeto: `Project Description.pdf`;
2. Arquivo RDATA contendo o Dataset do projeto: `dataset.RDATA`;
3. Modelo de machine learning supervisionado de classificação treinado para este dataset: `fit.svmLinear.RDS` 
4. Código contendo análise e criação do modelo machine learning em corpo: `ICU.R`;
5. Biblioteca R construída para esta análise e gráficos: `tools_package.R`;
6. Código contendo análise e gráficos através da biblioteca "tools_package": `ICU_by_tools_package.R`;
7. Código exemplificando o tratamento dos dados, predição via modelo de machine learning e criação do relatório através da bilbioteca "tools_package": `example_Treatment_ML_Report.R`;
8. Código com abrangendo relatório Rmardown "Rmd": `Report_ICU.RMD`;
9. Relatório no formato PDF ".pdf": `Report_ICU.pdf`;
10. Relatório no formato DOC ".docx": `Report_ICU.docx`;
11. Relatório no formato web ".HTML": `Report_ICU.html`.


#### OSB-01: Caso queira utilizar a renderização dos gráficos na criação de um relatório web (.HTML), por gentileza, efeuar os seguintes passsos:

1. Abrir o código R: `ICU_by_tools_package.R`;
2. Efetuar o comando: `CRTL + SHIFT + K`;
3. Escolher a saída: `HTML`.

Deste modo, um relatório web no seu brownser irá abrir.

#### OSB-02: Para criação dos relatórios através da biblioteca `tools_pachage.R`, por gentileza, utilizar o arquivo `Report_ICU.RMD` e a função `create_report` do pacote, 
passando como parâmetros o diretório do arquivo `Report_ICU.RMD` e o formato de saída, por exemplo: `html_document`, `word_document` ou `pdf_document`. Para o documento no formato .PDF é necessário a biblioteca LATEX instalada.

### ATENÇÂO: Mantenha os arquivos no mesmo diretório.
