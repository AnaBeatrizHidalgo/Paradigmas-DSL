# DSL CellLang

## Descrição Resumida da DSL

A proposta do projeto é desenvolver uma linguagem capaz de descrever cenários mais complexos de autômatos celulares, combinando múltiplas formas de vida e incorporando processos dinâmicos avançados por meio de operações e interações entre regras de comportamento. O objetivo é permitir a simulação de sistemas complexos mantendo uma sintaxe simples, expressiva e de fácil aprendizado.

A linguagem é implementada em Guile-Scheme e gera automaticamente um arquivo de configuração no formato .yml, que é então interpretado por um código em Python responsável pela simulação. A escrita do arquivo de configuração yml era complicada, por isso, fez se necessário a criação de uma linguagem facilitadora: O CellLang.

Além de possibilitar experimentos mais ricos nesse tipo de autômato celular, a linguagem busca aproximar temas de biologia e sistemas complexos do público em geral, oferecendo uma interface acessível mesmo a pessoas sem experiência prévia em programação, incentivando a exploração e criação autônoma de novos cenários.

## Slides

Link: https://www.canva.com/design/DAG2PleaBzc/cGhrHlZsyY5j5RrFE5_2aQ/edit?utm_content=DAG2PleaBzc&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton 

PDF: [Apresentação](Apresentacao_DSL.pdf)

## Notebook

Confira a linguagem e exemplos de uso em [notebock main](main.ipynb)

Aqui está a sintaxe do README atualizada para refletir o novo código, mantendo o formato e a especificidade da versão anterior.

## Sintaxe da Linguagem

### 1. Estrutura principal

#### Sintaxe geral

```scheme
(CREATE AUTOMATON
  (GRID <width> <height>)
  
  ;; Definições de Configuração
  (CONFIG <display-parameter>)
  ...
  
  ;; Definições de Funções
  (DEFINE FUNCTION <nome-funcao> <function-definition>)
  ...
  
  ;; Definições de Kernels
  (DEFINE KERNEL <nome-kernel> <kernel-definition>)
  ...
  
  ;; Definições de Formas de Vida
  (ADD LIFEFORM <nome-lifeform>
    <lifeform-properties> ...)
  ...)

;; Comando de execução
(run-simulation <automaton-definition>)
```

O bloco `CREATE AUTOMATON` define **todo o contexto** de um autômato celular.

  * O `GRID` é obrigatório e define o tamanho do autômato.
  * Os blocos `CONFIG`, `DEFINE FUNCTION`, `DEFINE KERNEL` e `ADD LIFEFORM` podem aparecer em qualquer ordem e substituem os antigos blocos agrupadores (`functions`, `kernels`, etc.).

-----

### 2\. Configuração da Grade (`GRID`)

#### Sintaxe

```scheme
(CREATE AUTOMATON
  (GRID <width> <height>)
  ...)
```

### Descrição

Define as dimensões da grade onde o autômato evolui. Esta é a **primeira expressão** dentro do `CREATE AUTOMATON`.

| Parâmetro | Tipo    | Descrição        |
| --------- | ------- | ---------------- |
| `width`   | inteiro | Largura da grade |
| `height`  | inteiro | Altura da grade  |

-----

### 3\. Definição de Funções (`DEFINE FUNCTION`)

As funções descrevem transformações contínuas ou discretas aplicadas às células.
Cada função é definida no escopo principal do `CREATE AUTOMATON`.

#### Sintaxe geral

```scheme
(DEFINE FUNCTION <nome> (<tipo> <parametros> ...))
```

#### Tipos suportados e suas formas

| Tipo         | Forma                                                                  | Descrição                                 |
| ------------ | ---------------------------------------------------------------------- | ----------------------------------------- |
| `gaussian`   | `(gaussian mu: <m> sigma: <s> amplitude: <amp> baseline: <base>)`      | Função gaussiana                          |
| `step`       | `(step threshold: <t> low_value: <lv> high_value: <hv>)`               | Função degrau                             |
| `linear`     | `(linear slope: <sl> intercept: <int>)`                                | Função linear (ax + b)                    |
| `conway`     | `(conway underpopulation: <und> overpopulation: <over> reproduction:<r>)` | Regras discretas de Conway (Game of Life) |
| `identity`   | `(identity)`                                                           | Função identidade (retorna a entrada)     |

#### Exemplo

```scheme
(CREATE AUTOMATON
  (GRID 100 100)
  (DEFINE FUNCTION f1 (gaussian mu: 0.3 sigma: 0.05 amplitude: 1.0 baseline: 0.0))
  (DEFINE FUNCTION f2 (step threshold: 0.2 low_value: 0.0 high_value: 1.0))
  ...)
```

-----

### 4\. Definição de Núcleos (`DEFINE KERNEL`)

Kernels representam vizinhanças espaciais ou filtros usados para convolução.
São definidos no escopo principal do `CREATE AUTOMATON`.

#### Sintaxe geral

```scheme
(DEFINE KERNEL <nome> (<tipo> <parametros> ...))
```

#### Tipos suportados

| Tipo     | Forma                                                              | Descrição                     |
| -------- | ------------------------------------------------------------------ | ----------------------------- |
| `disk`   | `(disk radius: <r> blur: (sigma: <s> size: <sz>))`                 | Disco gaussiano               |
| `ring`   | `(ring outer: <out> inner: <in> blur: (sigma: <s> size: <sz>))`    | Anel difuso                   |
| `square` | `(square side: <side> blur: (sigma: <s> size: <sz>) custom: <arr>)` | Quadrado (com filtro opcional) |

*Nota: O parâmetro `custom: <arr>` no `square` é opcional.*

#### Exemplo

```scheme
(CREATE AUTOMATON
  (GRID 100 100)
  (DEFINE KERNEL k1 (disk radius: 5 blur: (sigma: 1.5 size: 9)))
  (DEFINE KERNEL k2 (ring outer: 6 inner: 2 blur: (sigma: 2 size: 11)))
  ...)
```

-----

### 5\. Combinadores de Kernels

Operações de composição de kernels. Elas são usadas como a definição de um `DEFINE KERNEL`.

| Operador          | Descrição                        |
| ----------------- | -------------------------------- |
| `(kernel+ k1 k2)` | Soma dois kernels                |
| `(kernel- k1 k2)` | Subtrai (k1 - k2)                |
| `(kernel* k1 k2)` | Multiplica ponto a ponto         |
| `(kernel/ k1 k2)` | Divide ponto a ponto (k1 / k2)   |
| `(scale-kernel c)`| Retorna função que escala kernel por `c` |
| `'<nome>`         | Referência a kernel nomeado      |

*Nota: `k1` e `k2` podem ser referências (ex: `'k1`) ou definições de kernel inline (ex: `(define-kernel (disk ...))`).*

#### Exemplo

```scheme
(CREATE AUTOMATON
  (GRID 100 100)
  (DEFINE KERNEL k1 (disk radius: 5 blur: (sigma: 1.5 size: 9)))
  (DEFINE KERNEL k2 (ring outer: 6 inner: 2 blur: (sigma: 2 size: 11)))
  
  ;; k3 é a soma de k1 e k2
  (DEFINE KERNEL k3 (kernel+ 'k1 'k2))

  ;; k4 é uma combinação complexa
  (DEFINE KERNEL k4
    (kernel-
      (define-kernel (ring outer: 21 inner: 15 blur: (sigma: 1.2 size: 15))) 
      (kernel/ 'k1 2)))
  ...)
```

-----

### 6\. Definição de Formas de Vida (`ADD LIFEFORM`)

As **lifeforms** são entidades vivas com estado inicial, cor e regras de evolução.
São definidas no escopo principal do `CREATE AUTOMATON`.

#### Sintaxe

```scheme
(ADD LIFEFORM <nome>
  (color: <cor>)
  (initial: <inicialização>)
  (rules: <regra> ...))
```

#### Regras (`rule`)

Cada `rule` segue a forma:

```scheme
(rule <alvo> -> (dt: <Δt> kernel: <kernel> function: <função> weight: <peso>))
```

| Campo      | Descrição                                    |
| ---------- | -------------------------------------------- |
| `alvo`     | Nome da variável/camada alvo (geralmente o nome da própria lifeform) |
| `dt`       | Passo temporal da regra                      |
| `kernel`   | Nome (símbolo) do kernel a ser usado         |
| `function` | Nome (símbolo) da função a ser usada         |
| `weight`   | Peso multiplicativo da regra                 |

#### Exemplo

```scheme
(CREATE AUTOMATON
  (GRID 100 100)
  (DEFINE FUNCTION f1 ...)
  (DEFINE KERNEL k1 ...)
  
  (ADD LIFEFORM amoeba
    (color: "#00FF88")
    (initial: random)
    (rules:
      (rule amoeba -> (dt: 0.01 kernel: k1 function: f1 weight: 1.0))
      (rule amoeba -> (dt: 0.05 kernel: k2 function: f2 weight: 0.8))))
  ...)
```

-----

### 7\. Parâmetros de Configuração (`CONFIG`)

Controla a renderização e outros parâmetros globais. Substitui o antigo bloco `display` (e `simulation`).
Múltiplas cláusulas `CONFIG` podem ser usadas.

#### Sintaxe

Cada parâmetro é uma macro específica dentro de um `(CONFIG ...)`:

| Macro                     | Descrição                             |
| ------------------------- | --------------------------------------- |
| `(WINDOW <identificador>)`| Nome da janela (símbolo)                |
| `(FPS <valor>)`           | Taxa de atualização (quadros por segundo) |
| `(SCALE <valor>)`         | Escala do zoom da grade (inteiro)       |
| `(QUIT <tecla>)`          | Tecla de saída (símbolo, ex: `q`)       |

#### Exemplo

```scheme
(CREATE AUTOMATON
  (GRID 100 100)
  (CONFIG (WINDOW main))
  (CONFIG (FPS 60))
  (CONFIG (SCALE 2))
  (CONFIG (QUIT q))
  ...)
```

-----

### 8\. Execução e Saída

Funções usadas fora do bloco `CREATE AUTOMATON` para inspecionar ou executar a configuração.

#### Sintaxe

| Função                         | Descrição                                                                      |
| ------------------------------ | ------------------------------------------------------------------------------ |
| `(show-config <cfg>)`          | Imprime a configuração YAML resultante no console.                             |
| `(write-config-to-file <cfg> <arquivo>)` | Salva a configuração YAML em `<arquivo>`.                             |
| `(run-simulation <cfg>)`       | **Comando principal:** Salva a config em `temp.yml` e executa o simulador Python. |

*Onde `<cfg>` é a variável que armazena o resultado da macro `CREATE AUTOMATON`.*

#### Exemplo

```scheme
(define my-automaton
  (CREATE AUTOMATON
    (GRID 100 100)
    ...
  ))

;; Apenas imprime o YAML
(show-config my-automaton)

;; Salva o YAML
(write-config-to-file my-automaton "meu_automato.yml")

;; Executa a simulação
(run-simulation my-automaton)
```

-----

### 9\. Exemplo na linguagem

```scheme
;; Define a configuração do autômato 'nuclei'
(define nuclei
  (CREATE AUTOMATON
    (GRID 1080 720)
    (CONFIG (FPS 60))
    (CONFIG (WINDOW main))
    (CONFIG (QUIT q))
    
    (DEFINE FUNCTION f1
      (gaussian mu: 0.11 sigma: 0.08 amplitude: 2.0 baseline: -1.0))

    (DEFINE FUNCTION f2
      (gaussian mu: 0.9 sigma: 0.05 amplitude: 2.0 baseline: -1.0))
    
    (DEFINE KERNEL k1
      (ring outer: 41 inner: 31 blur: (sigma: 1.1 size: 13)))

    (DEFINE KERNEL k2
      (kernel-
        (define-kernel (ring outer: 21 inner: 15 blur: (sigma: 1.2 size: 15))) 
        (kernel/ 'k1 2)))
    
    (ADD LIFEFORM skin
      (color: "#28ff9bff")
      (initial: random)
      (rules:
        (rule skin -> (dt: 0.001 kernel: k2 function: f1 weight: 1.0))))
    
    (ADD LIFEFORM nucleo
      (color: "#e3b05eff")
      (initial: random)
      (rules:
        (rule nucleo -> (dt: 0.001 kernel: k2 function: f2 weight: 1.0))))))

;; Executa a simulação
(run-simulation nuclei)
```

## Exemplos Selecionados

> Coloque um conjunto de exemplos selecionados e os resultados alcançados.
### Exemplo 1

Você pode olhar o resultado da CellLang no arquivo [Exemplo 1](exemplo1.yml)

### Exemplo 2

Você pode olhar o resultado da CellLang no arquivo [Exemplo 2](exemplo2.yml)

### Exemplo 3

Você pode olhar o resultado da CellLang no arquivo [Exemplo 3](exemplo3.yml)

## Discussão

Percebemos que a interação entre formas com regras diferentes pode gerar resultados muito interessantes e muito complexos. Dessa forma, se conseguirmos terminar o CellLang e incrementá-lo de forma a tornar a criação de regras que fazem sentido simples e fácil, teremos uma forma simples de modelar diversos cenários diferentes (sejam reações químicas, interações de seres vivos, queimadas, etc).

## Conclusão

Honestamente, depois de fazer um projeto de DSL é perceptível como simples decisões de design podem ter impactos significantes na linguagem e como decisões para facilitar a vida do usuário podem facilmente se transformar em um development hell.

Outro aprendizado importante é como uma linguagem funcional com estruturas de dados extremamente simples é poderosa, mas ao mesmo tempo, depende muito de boas práticas de desenvolvimento para que você crie estruturas cada vez mais complexas no código de fácil uso e melhoria.

Acreditamos que a principal lição aprendida é que o desenvolvimento da linguagem deve ser mais iterativo, com rodadas mais extensivas de teste para encontrar mais facilmente edge-cases no seu desenvolvimento e escutar opiniões de outras pessoas de como a linguagem faz mais sentido de ser apresentada.  

# Trabalhos Futuros

* Correções na geração do .yml para a geração dos jogos ainda são necessárias, além de que as operações de composição de kernel ainda necessitam de correção pois apresentam muitos bugs.

* A adição de regras ainda mais complexas facilmente modularizadas, permitindo a criação de outros cenários. Por exemplo: novas operações de mutualismo ou regras diferentes de divisão.

# Testes

Os .yml gerados ainda não estão no formato necessário para ser consumidos pelo código de python, mas para testar em arquivos que funcionem, crie um ambiente virtual de python com o arquivo `requirements.txt` e rode: 

```bash
python yaml_main.py my_config.yml
```

# Guia para desenvolvimento

Para facilitar o desenvolvimento recomendo, instale localmente o guile para notebook como
o tutorial mostra em 

Alternativa mais simples é instalar o guile via um package manager, 
```bash
# apt
apt install guile-3.0

#pacman
pacman -S guile
```

e trabalhar em arquivos `.scm`.

# Referências Bibliográficas

- **Lenia**: [Paper by Bert Wang-Chak Chan](https://arxiv.org/abs/1812.05433)
- **Conway's Game of Life**: [Wikipedia](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
- **Cellular Automata**: [Wolfram MathWorld](https://mathworld.wolfram.com/CellularAutomaton.html)
- **Repositório base de Automato celular**: [Repo](https://github.com/LucasPeixotg/continuous_automata#)
