
# Pacote tjsp

## Baixando e lendo decisões de primeiro e segundo grau.

O objetivo deste pacote é disponibilizar ferramentas a acadêmicos,
jornalistas e organizações não governamentais para coletar e organizar
decisões judiciais de primeira e de segunda instância do Tribunal de
Justiça de São Paulo.

## Instalação

Este pacote não será incluído no cran. Você deverá fazer uso da versão
em desenvolvimento. Há pelo menos duas razões para não incluir no cran:

1 - Pacotes para web scraping estão em constante desenvolvimento.
Pǻginas web são repentinamente alteradas ou mesmo inteiramente
substituídas, o que pode obrigar o desenvolvedor a reescrever o pacote
do zero;

2 - Os usuários do pacote serão em sua maioria brasileiros, e mesmo que
não lusófonos venham a utilizá-lo, a familiaridade com a lingua
portuguesa e com o direito brasileiro é indispensável;

Portanto, instale versão em desenvolvimento:
[GitHub](https://github.com/) com:

``` r
install.packages("remotes")
remotes::install_github("jjesusfilho/tjsp")
```

## Utilização

As funções do pacote podem ser agrupadas em três. Um grupo de funções
que inicia com o verbo baixar, as quais baixam para um diretório
indicado pelo usuário, as decisões de primeira e de segunda instância em
formato html.

O segundo grupo inicia com o verbo ler, as quais leem as informações
contidas nos htmls e as dispôem numa tabela. Todas as funções de leitura
são paralelizadas, de modo que o tempo de leitura dependerá da
configuração de sua máquina.

O terceiro grupo é formado por funções auxiliares no trabalho de
transformação dos dados lidos pelo grupo anterior.

Há quatro siglas que você deve tomar com conta antes de baixar os dados
processuais. A sigla `cjpg` significa consulta de julgados de primeiro
grau, ela basicamente baixa dados do [banco de
sentenças](http://esaj.tjsp.jus.br/cjpg/). A sigla `cjsg` signfica
consulta consulta de julgados de segundo grau e baixa os [julgados de
segundo grau](https://esaj.tjsp.jus.br/cjsg/consultaCompleta.do). A
sigla `cpopg` significa consulta processual de primeiro grau e baixa a
[consulta processual de primeiro
grau](https://esaj.tjsp.jus.br/cpopg/open.do). Por fim, a sigla `cposg`
significa consulta processual de segundo grau e baixa a [consulta
processual de segundo grau](https://esaj.tjsp.jus.br/cposg/open.do).

### Baixando jurisprudência

As decisões de segunda instância podem ser consultadas livremente por
meio da [página jurisprudência do
TJSP](https://esaj.tjsp.jus.br/cjsg/consultaCompleta.do?f=1). Por
exemplo, para realizar uma busca livre sobre o tema feminicídio, faça o
seguinte:

``` r
library(tjsp)
dir.create("feminicidio")
tjsp_baixar_cjsg(livre="feminicídio", diretorio="feminicidio")
```

Ela baixará no diretório indicado ou no atual, os htmls com os metadados
das decisões. Atenção, faça a busca no TJ antes para verificar quantas
páginas serão baixadas. A depender do tema, esse processo pode tomar
horas.

Depois disso, você pode pedir para ler tais decisões:

``` r
tabela <- tjsp_ler_cjsg(diretorio = "feminicidio")
```

### Baixando informações detalhadas dos processos

O passo seguinte é realizar a busca e baixar os htmls dos processos
individualmente considerados. Recentemente o TJSP impôs recaptcha para
baixar os processos. Diante disso, para baixar processos você deve antes
se autenticar como advogado. Por meio da função `autenticar()`, você
será solicitado a apresentar suas credenciais (CPF e senha), a fim de
ter acesso aos processos sem o uso de captcha.

O comando a seguir irá baixar todos os processos no diretório atual, mas
você pode indicar um diretóio de sua escollha.

``` r
tjsp_baixar_cposg(tabela$processo)
```

### Lendo os processos de segunda instância.

A leitura dos processos de segunda instância se dá em três etapas.
Primeiramente, lemos os metadados:

``` r
dados <- tjsp_ler_dados_cposg(diretorio = ".")
```

Em seguida, lemos as informações acerca das partes dos processos:

``` r
partes <- tjsp_ler_partes(diretorio = ".")
```

Ao final passamos para a leitura do andamento dos processos.

``` r
andamento <- tjsp_ler_movimentacao_cposg(diretorio = ".")
```

Eventualmente, você não está interessada em ler todo o andamento, mas
somente a data da entrada do processo em segunda instância para mais
tarde calcular o tempo entre a entrada e a decisão. Há uma função para
isso:

``` r
entrada <- tjsp_ler_entrada_cposg(diretorio = ".")
```

Por fim, você pode ler o dispositivo da decisão:

``` r
decisao <- tjsp_ler_dispositivo(diretorio = ".")
```

### Baixando decisões de primeiro grau

As decisões de primeiro grau obedecem a mesma lógica das decisões de
segundo grau com algumas diferenças. Verifique a ajuda para entender
como operam.

## Aviso legal (Disclaimer)

O pacote tjsp não mantêm nem atualiza os dados do TJSP/ESAJ, não altera
nem transforma os dados durante a coleta. Algumas funções, tais como
classificar_recurso, classificar_sentença, numero e votacao criam novas
variáveis a partir das existentes e devem ser usadas por conta e risco
de quem utiliza o pacote. Elas não estão isentas de erro e seus
resultados devem ser validados. O autor do pacote não assume qualquer
responsabilidade sobre o seu uso e resultados.

## Considerações éticas

1 - Este pacote foi criado por colaboradores voluntários. Você também é
convidado a contribuir. Se encontrar uma falha, não hesite em criar um
“issue”, mas também procure fazer um esforço para corrigir você mesma(o)
o erro e dar um “pull request”;

2 - O pacote é destinado principalmente ao público acadêmico,
jornalistas e membros de organizações sem fins econômicos.

3 - Use o pacote com parcimônia. Não bombardeie a página do TJSP, pois a
navegação de milhares de usuários poderá ser afetada. Você realmente
precisa baixar tantas decisões? Precisa ser durante o dia, não pode ser
à noite? Propositalmente, não incluímos opções de requisições
assincrônicas ou paralelas.

4 - Todas as funções de requisição possuem um argumento opcional chamado
“usuario”. Ele pode ser usado para você identificar-se ao provedor do
serviço, fornecendo o seu contato. Essa tem sido uma prática considerada
ética por web scrapers.

6 - Idealmente, o TJSP deveria disponibilizar uma Web API ou no mínimo
um web service para facilitar o acesso a grandes volumes de decisões via
comunicação máquina-máquina. Isso não custa tanto e não estaríamos
criando esse raspador, como seguramente há muitos outros especialmente
privados trabalhando nesse momento para fins comerciais. No entanto, o
TJSP não proíbe expressamente o uso de raspadores, você pode conferir o
que estou dizendo acessando o robots.txt do TJSP. Isso porém não quer
dizer que seu IP não será bloqueado caso você decida reduzir o tempo
entre requisições.

7 - Use os dados baixados para agregar valor, como por exemplo, para
realizar análises ou publicar papers, fazer inferências, elaborar
recomendações aos poderes públicos etc. Baixar esses dados para
reproduzi-los em sua página web é tirar proveito do trabalho alheio,
mesmo sendo esses dados públicos.
