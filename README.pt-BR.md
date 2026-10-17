[English](README.md) · Português

# Yeditor

[![CI](https://github.com/Yuri-Albuquerque/yeditor/actions/workflows/test.yml/badge.svg)](https://github.com/Yuri-Albuquerque/yeditor/actions/workflows/test.yml)

Yeditor é um **editor de código composável espaço-temporalmente, agente**, construído em Common Lisp, derivado de [Lem](https://github.com/lem-project/lem) (MIT).

## Por que Yeditor

### Easy as VSCode

- Dashboard; Command Palette com busca difusa (Ctrl-Shift-P, unificado com M-x); configurações em `.yeditor.lisp` com auto-reload (HMR); LSP auto-configurado por linguagem; UI limpa (zero chrome)

### Productive as Emacs

- Bindings de teclado estilo Emacs; macros; menus transitórios; REPL integrado; o editor inteiro é um REPL; tudo scriptável em Common Lisp

### Easy Ollama/Claude integration

- `le-ollama` (detecta automaticamente servidor em 127.0.0.1:11434; baixa modelo dentro do editor; chat inline; explica/reescreve/documenta);
- `le-claude-bridge`;
- **Servidor MCP reutilizado do Lem** (`claude mcp add --transport http lem http://localhost:7890/mcp`)

## A ideia central: efeitos reversíveis e co-efeitos reativos

Cada mutação do editor é uma **transação** que carrega seu inverso (capturado no momento da instalação). Mudanças de contexto de componente são classificadas contra **co-efeitos** declarados (modo-buffer, tipo-projeto…) que governam a ativação. O paradigma segue "A Programming Paradigm for Spatiotemporal Composability" (Shi/Zhang/Cui, 2026, arXiv:2608.25512).

## Status

### O que funciona hoje (herdado do Lem)

- Frontends ncurses/webview/SDL2, LSP, tree-sitter, cliente-servidor, servidor MCP, modo Copilot, +180 temas (SDL2), 12 mil commits de herança upstream.

### O que estamos construindo (Fase 1)

- Núcleo transacional — `yeditor-tx` (log de efeitos reversíveis, grupos de undo, rollback, durabilidade), `yeditor-context` (contexto unificado de efeito/co-efeito), `yeditor-sdk` (DSL define-component), inversos padrão para 15 tipos de efeito.

## O que o Yeditor NÃO é

- NOT a visual VSCode clone — teclado-primeiro com onboarding estilo VSCode
- DOES NOT run pacotes ELisp (linguagem diferente; camada de compatibilidade de esforço futuro)
- NOT a general plugin framework for arbitrary apps — um editor; `yeditor-tx`/`yeditor-context` podem ser extraídos como bibliotecas ASDF independentes mais tarde

## Instalação

Yeditor acompanha upstream Lem; a instalação segue os mesmos caminhos que o Lem. Veja a documentação upstream do Lem (https://lem-project.github.io/) e as compilações nightly em https://github.com/lem-project/lem/releases/tag/nightly-latest.

## Roteiro

- Fase 0 — Fork + CI verde no primeiro build (Out 2026)
- Fase 1 — Núcleo transacional M1-M5, T1-T10 (Nov 2026 – Mar 2027)
- Fase 2 — Ecossistema de componentes, T11-T15 (Jun 2027)
- Fase 3 — IA + forge (T16-T19, Set 2027)
- Fase 4 — Velocidade percebida + polimento (Nov 2027)

## Contribuição

Veja [CONTRIBUTING.md](CONTRIBUTING.md).

## Citação

```bibtex
@misc{shi2026cordis,
  title         = {A Programming Paradigm for Spatiotemporal Composability},
  author        = {Shi, Yifan and Zhang, Wei and Cui, Tianyi},
  year          = {2026},
  eprint        = {2608.25512},
  archivePrefix = {arXiv},
  primaryClass  = {cs.PL},
  url           = {https://arxiv.org/abs/2608.25512},
}
```

Baseado no Lem (lem-project/lem), MIT — 12 099 commits de herança upstream.
