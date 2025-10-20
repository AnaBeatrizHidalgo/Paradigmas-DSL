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

## Sintaxe da Linguagem

### 1. Estrutura principal

#### Sintaxe geral

```scheme
(automaton
  (grid <width> <height>)
  (functions <function-definition> ...)
  (kernels <kernel-definition> ...)
  (lifeforms <lifeform-definition> ...)
  (simulation <simulation-parameter> ...)
  (display <display-parameter> ...))
```

O bloco `automaton` define **todo o contexto** de um autômato celular.
Cada subbloco (`grid`, `functions`, `kernels`, etc.) é obrigatório e deve seguir as formas descritas abaixo.

---

### 2. Configuração da Grade (`grid`)

#### Sintaxe

```scheme
(grid <width> <height>)
```

### Descrição

Define as dimensões da grade onde o autômato evolui.

| Parâmetro | Tipo    | Descrição        |
| --------- | ------- | ---------------- |
| `width`   | inteiro | Largura da grade |
| `height`  | inteiro | Altura da grade  |

---

### 3. Definição de Funções (`define-function`)

As funções descrevem transformações contínuas ou discretas aplicadas às células.
Cada função recebe um **nome** e uma **configuração de parâmetros nomeados**.

#### Sintaxe geral

```scheme
(define-function <nome> (<tipo> <parametros> ...))
```

#### Tipos suportados e suas formas

| Tipo         | Forma                                         | Descrição                            |
| ------------ | --------------------------------------------- | ------------------------------------ |
| `gaussian`   | `(gaussian mean: <m> sigma: <s> growth: <g>)` | Função gaussiana                     |
| `step`       | `(step threshold: <t> slope: <a>)`            | Função degrau suave                  |
| `lenia`      | `(lenia mu: <m> sigma: <s> beta: <b>)`        | Função típica de Lenia               |
| `regression` | `(regression a: <a> b: <b> c: <c>)`           | Polinômio de regressão de ordem 2    |
| `sigmoid`    | `(sigmoid k: <k> x0: <x0>)`                   | Função sigmoide                      |
| `tanh`       | `(tanh scale: <s> bias: <b>)`                 | Função tangente hiperbólica escalada |

#### Exemplo

```scheme
(define-function f1 (gaussian mean: 0.3 sigma: 0.05 growth: 1.0))
(define-function f2 (step threshold: 0.2 slope: 5))
```

---

### 4. Definição de Núcleos (`define-kernel`)

Kernels representam vizinhanças espaciais ou filtros usados para convolução.

#### Sintaxe geral

```scheme
(define-kernel <nome> (<tipo> <parametros> ...))
```

#### Tipos suportados

| Tipo                              | Forma                                                               | Descrição                       |
| --------------------------------- | ------------------------------------------------------------------- | ------------------------------- |
| `disk`                            | `(disk radius: <r> blur: (sigma: <s> size: <sz>))`                  | Disco gaussiano                 |
| `ring`                            | `(ring outer: <out> inner: <in> blur: (sigma: <s> size: <sz>))`     | Anel difuso                     |
| `square`                          | `(square side: <side> blur: (sigma: <s> size: <sz>))`               | Quadrado gaussiano              |
| `square` (com matriz customizada) | `(square side: <side> blur: (sigma: <s> size: <sz>) custom: <arr>)` | Quadrado com filtro customizado |

#### Exemplo

```scheme
(define-kernel k1 (disk radius: 5 blur: (sigma: 1.5 size: 9)))
(define-kernel k2 (ring outer: 6 inner: 2 blur: (sigma: 2 size: 11)))
```

---

### 5. Combinadores de Kernels

Operações de composição de kernels:

| Operador            | Descrição                                |
| ------------------- | ---------------------------------------- |
| `(kernel+ k1 k2)`   | Soma dois kernels                        |
| `(kernel- k1 k2)`   | Subtrai                                  |
| `(kernel* k1 k2)`   | Multiplica ponto a ponto                 |
| `(kernel/ k1 k2)`   | Divide ponto a ponto                     |
| `(scale-kernel c)`  | Retorna função que escala kernel por `c` |
| `(kernel-ref nome)` | Referência a kernel nomeado              |

#### Exemplo

```scheme
(define combined (kernel+ (kernel-ref k1) (kernel-ref k2)))
```

---

### 6. Definição de Formas de Vida (`define-lifeform`)

As **lifeforms** são entidades vivas com estado inicial, cor e regras de evolução.

#### Sintaxe

```scheme
(define-lifeform <nome>
  (color: <cor>)
  (initial: <inicialização>)
  (rules: <regra> ...))
```

#### Regras (`rule`)

Cada `rule` segue a forma:

```scheme
(rule <alvo> -> (dt: <Δt> kernel: <kernel> function: <função> weight: <peso>))
```

| Campo      | Descrição                     |
| ---------- | ----------------------------- |
| `alvo`     | Nome da variável/camada alvo  |
| `dt`       | Passo temporal                |
| `kernel`   | Kernel usado na convolução    |
| `function` | Função usada na transformação |
| `weight`   | Peso multiplicativo           |

#### Exemplo

```scheme
(define-lifeform amoeba
  (color: "blue")
  (initial: seed)
  (rules:
    (rule density -> (dt: 0.1 kernel: k1 function: f1 weight: 1.0))
    (rule energy -> (dt: 0.05 kernel: k2 function: f2 weight: 0.8))))
```

---

### 7. Parâmetros de Simulação (`simulation`)

Define parâmetros globais do sistema dinâmico.

#### Sintaxe

Cada parâmetro é um par nome-valor com macro específica:

| Macro             | Descrição                 |
| ----------------- | ------------------------- |
| `(dt <v>)`        | Passo de tempo            |
| `(steps <n>)`     | Número total de iterações |
| `(seed <n>)`      | Semente de aleatoriedade  |
| `(diffusion <v>)` | Taxa de difusão           |

#### Exemplo

```scheme
(simulation
  (dt 0.1)
  (steps 10000)
  (seed 42)
  (diffusion 0.2))
```

---

### 8. Parâmetros de Exibição (`display`)

Controla a renderização do autômato.

#### Sintaxe

| Macro                      | Descrição                                 |
| -------------------------- | ----------------------------------------- |
| `(window <identificador>)` | Nome da janela                            |
| `(fps <valor>)`            | Taxa de atualização (quadros por segundo) |
| `(scale <valor>)`          | Escala do zoom da grade                   |
| `(quit <tecla>)`           | Tecla de saída                            |

#### Exemplo

```scheme
(display
  (window main)
  (fps 60)
  (scale 2)
  (quit q))
```

---

### 9. Ver a configuração (show-config)

No estado atual da linguagem, ainda não conseguimos escrever diretamente no .yml ou criar o .yml, então temos uma função que imprime a configuração no formato do .yml 

#### Sintaxe

| Parâmetro                  | Descrição                                 |
| -------------------------- | ----------------------------------------- |
| `(config)`                 | Confifuração do automato                  |

#### Exemplo

```scheme
(show-config cfg)
```

### 10. Exemplo na linguagem

```scheme
(automaton
  (grid 128 128)
  (functions
    (define-function f1 (gaussian mean: 0.3 sigma: 0.05 growth: 1.0))
    (define-function f2 (step threshold: 0.2 slope: 5)))
  (kernels
    (define-kernel k1 (disk radius: 5 blur: (sigma: 1.5 size: 9)))
    (define-kernel k2 (ring outer: 6 inner: 2 blur: (sigma: 2 size: 11))))
  (lifeforms
    (define-lifeform amoeba
      (color: "green")
      (initial: seed)
      (rules:
        (rule density -> (dt: 0.1 kernel: k1 function: f1 weight: 1.0))
        (rule energy -> (dt: 0.05 kernel: k2 function: f2 weight: 0.8)))))
  (simulation
    (dt 0.1)
    (steps 5000)
    (seed 42))
  (display
    (window main)
    (fps 60)
    (scale 2)
    (quit q)))

(show-config automaton)
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

> Discussão dos resultados. Relacionar os resultados com as perguntas de pesquisa ou hipóteses avaliadas.
>
> A discussão dos resultados também pode ser feita opcionalmente na seção de Resultados, na medida em que os resultados são apresentados. Aspectos importantes a serem discutidos: Por que seu modelo alcançou (ou não) um bom resultado? É possível tirar conclusões dos resultados? Quais? Há indicações de direções para estudo? São necessários trabalhos mais profundos?

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

# Referências Bibliográficas

- **Lenia**: [Paper by Bert Wang-Chak Chan](https://arxiv.org/abs/1812.05433)
- **Conway's Game of Life**: [Wikipedia](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
- **Cellular Automata**: [Wolfram MathWorld](https://mathworld.wolfram.com/CellularAutomaton.html)
- **Repositório base de Automato celular**: [Repo](https://github.com/LucasPeixotg/continuous_automata#)
