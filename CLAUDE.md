# Convenções de trabalho neste repositório

## Commit e push

- Ao concluir cada tarefa lógica (uma funcionalidade, correção, ajuste de
  documentação, etc.), crie um commit imediatamente e em seguida faça push
  para o branch remoto correspondente. Não acumule várias tarefas lógicas
  distintas em um único commit.
- Mensagens de commit seguem o padrão observado no histórico deste repositório:
  `Tipo: descrição breve em português`, em que `Tipo` é um dos seguintes,
  com a primeira letra maiúscula:
  - `Feat`: nova funcionalidade
  - `Fix`: correção de bug
  - `Docs`: alterações de documentação (README, roxygen, etc.)
  - `Refactor`: reorganização de código sem mudança de comportamento
  - Exemplos reais do histórico: `Feat: adiciona eproc`,
    `Fix: corrige tjsp_baixar_doc_completo.R`.
- A descrição é curta (uma linha), em português, no infinitivo ou no presente
  do indicativo, descrevendo o que a mudança faz.
- Antes de commitar, gere/atualize a documentação roxygen2 quando funções
  exportadas tiverem sido alteradas (`roxygen2::roxygenise()`).
