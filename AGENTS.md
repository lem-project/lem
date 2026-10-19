# CONTEXTO OPERACIONAL E ARQUITETURAL DO PROJETO: YEDITOR (GUIA PARA O AGENTE CODEX)

> Nota sobre o estado atual: este documento registra a direção de produto e o fluxo de trabalho solicitados pelo mantenedor; nomes de camadas e capacidades planejadas não comprovam implementação. Confira código, testes e documentação antes de declarar uma funcionalidade pronta.
>
> Licenciamento: confira `LICENSE`, `LICENSE-FSL`, `LICENSE-ENTERPRISE` e `THIRD_PARTY_NOTICES.md` na branch de destino. Preserve as licenças e os avisos existentes; não relicencie arquivos durante uma integração. O protótipo independente em `contrib/transaction-core-prototype/` mantém sua licença MIT.
>
> Ambiente na nuvem: use o checkout existente nesta tarefa isolada; não crie worktrees sem pedido explícito. Quando disponível, carregue `/workspace/.cloud-tools/activate.sh`. O acesso ao Quicklisp e os testes T1–T3 foram validados nesta sessão; não declare outros builds ou testes aprovados sem executá-los. Este arquivo orienta tarefas futuras e não inicia sozinho um loop contínuo de issues.

## 1. Visão Geral e Propósito do Produto
Você está trabalhando no repositório do **Yeditor** (`github.com/Yuri-Albuquerque/yeditor`), um ambiente de desenvolvimento e orquestração agêntica construído em **Common Lisp (SBCL)** sobre uma base derivada do **Lem**, adaptado ao paradigma matemático de **Composabilidade Espaço-Temporal** (baseado no paper do Cordis, arXiv:2608.25512) e projetado para **Operação Guiada por Intenção e Mouse ("Director Mode")**.

### Perfil de Uso e Filosofia de UX (Regra de Ouro)
O Yeditor NÃO é um editor tradicional dependente de digitação rápida ou memorização de acordes complexos de teclado estilo Emacs dos anos 1970 (`C-x`, `M-x`). Ele foi concebido para pesquisadores, matemáticos aplicados e arquitetos de software criativos que operam em alto nível de abstração:
1. **Intent-Driven & Agent-Operated:** O humano expressa a intenção (via Omnibar, clique de mouse ou voz) e os agentes (Codex, Hermes, Claude Code, Ollama) operam tanto o código quanto a própria interface do editor (abrir arquivos, dividir janelas, rodar o REPL, destacar regiões).
2. **UI Efêmera Dirigida por Coefeitos ("Zero-Chrome Reativo"):** Em repouso, a tela é 100% focada no código/visualização sem poluição visual. Painéis de diff, resultados de execução e docks de aprovação (`[✓ Commit]` / `[↺ Rollback]`) surgem automaticamente quando o coefeito correspondente (ex.: `:agent-tx-active`) está ativo e desaparecem assim que a decisão é tomada.
3. **Ergonomia Visual Anti-Halation:** Proibição estrita de preto puro (`#000000`) e branco puro (`#FFFFFF`). O padrão de fábrica utiliza tons quentes e contrastes controlados (ex.: **Gruvbox Soft** `#32302f` com texto `#ebdbb2`), tipografia peso 400–500 e `line-height: 1.6`.

---

## 2. Mapa Real do Repositório Atual (O que já existe e deve ser respeitado)
Antes de iniciar qualquer issue, consulte e respeite os artefatos já presentes na árvore do repositório:

1. **Governança, Estilo e Regras Locais:**
   - `.specify/memory/constitution.md` e templates em `.specify/templates/`
   - `.claude/rules/lisp-style.md` (convenções obrigatórias de Common Lisp)
   - `.claude/rules/frontend.md` (regras de interface e frontends)
   - `.claude/rules/testing.md` (padrões de testes automatizados)
   - `STYLEGUIDE.md`, `CLAUDE.md` e `contract.yml` na raiz.
2. **Documentação de Arquitetura:**
   - `docs/ARCHITECTURE.md` e `docs/arch.md` (visão em camadas L0–L5, protocolo de efeitos revertíveis).
   - `docs/LIVING-CANVAS-DESIGN.md` e `docs/LIVING-CANVAS-PHASE2-PLAN.md` (design visual interativo e manipulação espacial de código/grafos).
3. **Módulos e Extensões Chave Existentes:**
   - `extensions/mcp-server/`: Servidor MCP embutido (handlers para `buffer.lisp`, `command.lisp`, `display.lisp`, `editing.lisp`, `input.lisp`, `tools.lisp`).
   - `extensions/claude-code/`: Integração com SDK/CLI de agentes (`claude-code.lisp`, `claude-code-sdk.lisp`).
   - `extensions/living-canvas/` e `extensions/call-graph/`: Visualização viva de grafos de chamadas e providers para Common Lisp (`micros-cl-provider.lisp`), Python e JS.
   - `contrib/ollama/`: Cliente e listener local para modelos Ollama.
   - `frontends/`: `ncurses/`, `sdl2/`, `pdcurses/` e `fake-interface/` (usada para testes headless).

---

## 3. Arquitetura em Camadas e Modelo de Licenciamento (Open-Core)

O sistema obedece a uma hierarquia estrita de dependências (camadas superiores nunca são importadas por camadas inferiores, e mutações de estado global devem passar pelo protocolo transacional):

- **L0 — Substrato Base (`src/`, `frontends/`, `modes/`):** Base herdada do Lem (Licença **MIT** — manter `THIRD_PARTY_NOTICES.md` e avisos originais intactos; fazer o mínimo possível de alterações invasivas em L0 para evitar conflitos com upstream).
- **L1 — Núcleo Transacional e Memória (`yeditor-tx`, `yeditor-journal`):**
  - Toda mutação do editor ou de um agente (criar buffer, editar texto, adicionar hook, mudar keybinding, abrir processo, dividir janela) é um **efeito reversível** com um inverso capturado no ato da instalação.
  - Rollback desfaz todos os efeitos em ordem **LIFO** estrita ($f \circ f^{-1} = \text{id}$).
  - `yeditor-journal`: Log append-only (JSONL) de edições humanas/agênticas e avaliações científicas no REPL (*Eval-to-Journal*), garantindo contexto zero copy-paste para os agentes.
- **L2 — Contexto Unificado e Coefeitos Reativos (`yeditor-context`):**
  - Gerencia predicados de ativação espaço-temporal (`:buffer-mode`, `:project-type`, `:agent-tx-active`, `:eval-overlay-active`).
- **L3 — SDK, Componentes e Cockpit (`yeditor-sdk`, `components/`, `le-director-cockpit`, `living-canvas`):**
  - Componentes toggláveis em runtime sem resíduos de memória/estado (`define-component`).
  - Camadas L1–L3 de componentes base seguem licença **FSL-1.1-MIT** (*Functional Source License*).
- **L4 & L5 — Orquestração Agêntica, Branching Transacional e Forge (`yeditor/enterprise/`):**
  - Barramento agêntico (`le-agent-host`), ferramentas de controle do editor pelo agente (`editor/*`), bifurcação de transações em memória (testar 2 agentes em paralelo) e geração de componentes verificados (`le-forge`).
  - Licenciado sob **Licença Proprietária Comercial (`LicenseRef-Yeditor-Enterprise` — All Rights Reserved para Yuri Flores Albuquerque)**.

---

## 4. Fluxo de Trabalho para Puxar e Executar Issues (Loop Autônomo do Codex)

Sempre que você for instruído a avançar no projeto ou puxar uma issue do repositório, siga **exatamente** este protocolo em 6 etapas:

### Etapa 1: Seleção e Leitura de Contexto
1. Liste as issues abertas no GitHub (`gh issue list --state open`) priorizando pela ordem das fases:
   - `phase/0-foundation` $\rightarrow$ `phase/1-transaction-core` (T1–T10) $\rightarrow$ `phase/2-components` & `Director Cockpit` (T11–T15, T12-B, Living Canvas) $\rightarrow$ `phase/3-agentic` (T16–T25) $\rightarrow$ `phase/4-polish` (T26+).
2. Ao selecionar uma issue `TX`, leia:
   - A descrição completa da issue (`gh issue view <num>`).
   - As regras em `.claude/rules/lisp-style.md`, `.claude/rules/testing.md` e `.claude/rules/frontend.md`.
   - Os arquivos de design relevantes em `docs/` (`arch.md`, `ARCHITECTURE.md`, `LIVING-CANVAS-DESIGN.md`).

### Etapa 2: Preparação de Branch Isolada
1. Crie uma branch semântica limpa a partir da branch principal atualizada:
   - Exemplo: `git checkout -b feat/T1-transactional-boot` ou `feat/issue-<num>-<slug>`.

### Etapa 3: Desenvolvimento Guiado por Testes (Strict TDD)
1. **Escreva o Teste Primeiro (Red):**
   - Crie ou atualize o arquivo de teste correspondente (usando `rove` / `lem-fake-interface` conforme `.claude/rules/testing.md`).
   - Para qualquer componente ou efeito transacional, inclua obrigatoriamente o **Teste de Propriedade de Revertibilidade**: instalar $\rightarrow$ executar toggle/rollback $N$ vezes ($N \ge 100$) $\rightarrow$ provar por asserção que o estado global (buffers, hooks, keymaps, processos, variáveis) é idêntico ao estado inicial (zero leaks).
2. **Implementação Mínima e Limpa (Green):**
   - Escreva o código Common Lisp aderindo ao `STYLEGUIDE.md` e `.claude/rules/lisp-style.md`.
   - Adicione o cabeçalho de copyright/SPDX correto no topo de cada arquivo novo (`FSL-1.1-MIT` para core/componentes L1–L3; `LicenseRef-Yeditor-Enterprise` para módulos agênticos/enterprise L4–L5).
   - Nunca bloqueie a thread de UI; operações de I/O, processos externos ou chamadas a LLMs devem ser assíncronas.
3. **Verificação Local:**
   - Execute a suíte de testes e verifique se o build (como o target ncurses e os testes ASDF em `.github/workflows/yeditor-ci.yml`) passa com zero erros e zero warnings fatais do SBCL.

### Etapa 4: Prioridade Especial — "Agent-Operated & Mouse-First UX"
Ao implementar qualquer funcionalidade de interface, extensão (incluindo melhorias no `living-canvas` ou `mcp-server`) ou comando do editor:
1. **Exponha via MCP / Agent Tools (`extensions/mcp-server/tools.lisp`):** Garanta que o agente possa invocar o comando por conta própria para assistir o usuário.
2. **Garanta Ação por Clique / Intenção:** Todo recurso novo deve poder ser acionado via clique do mouse (Action Pills / Cockpit Dock / Living Canvas) ou por frase em linguagem natural na Intent Omnibar, sem exigir que o usuário decore atalhos de teclado.

### Etapa 5: Auto-Auditoria de Qualidade (Checklist Pré-Commit)
Antes de realizar o commit, verifique:
- [ ] Todos os efeitos registrados possuem inversos (`undo-thunk` ou inverso automático)?
- [ ] Se houver cores/CSS/faces envolvidas, elas passam na regra Anti-Halation (sem `#000000` ou `#FFFFFF`)?
- [ ] Os cabeçalhos de licença e a separação entre `L0` (MIT) e `yeditor/` estão preservados?
- [ ] Os testes automatizados da tarefa e a suíte existente passaram?

### Etapa 6: Commit, Pull Request e Próxima Issue
1. Faça commits atômicos e bem descritos referenciando a issue (`feat(tx): implement LIFO rollback for buffer effects (Closes #X)`).
2. Preencha os critérios do template `.github/PULL_REQUEST_TEMPLATE/ai_pull_request_template.md`.
3. Abra o Pull Request (ou consolide na branch de trabalho conforme instruído pelo usuário), reporte um resumo executivo curto do que foi feito e pergunte se deve puxar imediatamente a próxima issue da fila.

