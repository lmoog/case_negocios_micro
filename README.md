# Avaliação Causal de Programa de Incentivo – Case de Negócio

## Objetivo
Este projeto analisa o impacto de um programa piloto de incentivo ao consumo implementado em um subconjunto de cidades.

O objetivo é medir se a intervenção aumentou pedidos e receita por cliente, identificar possíveis efeitos de canibalização e entender para quais perfis de usuários o programa funciona melhor.

---

## Pergunta de Negócio
O piloto gerou crescimento real de vendas ou apenas deslocou consumo que ocorreria de qualquer forma?

Além disso: quais segmentos respondem mais fortemente ao incentivo?

---

## Dados
- Painel de usuários ao longo do tempo  
- Informações de pedidos e valores gastos  
- Identificação de cidades elegíveis ao piloto  
- Histórico pré e pós intervenção  

---

## Metodologia
- Difference-in-Differences com efeitos fixos de usuário e período  
- Comparação entre áreas tratadas e não tratadas  
- Erros padrão clusterizados por cidade  
- Estudo de evento para teste de tendências paralelas  
- Análise de heterogeneidade por intensidade de uso  

---

## Estrutura do Repositório
