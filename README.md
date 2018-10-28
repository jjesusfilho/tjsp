
<!-- README.md is generated from README.Rmd. Please edit that file -->
tjsp
====

O objetivo deste pacote é disponibilizar ferramentas a acadêmicos, jornalistas e organicações não governamentais para baixar e organizar decisões judiciais de primeira e segunda instância do Tribunal de Justiça de São Paulo.

Instalação
----------

Este pacote não será incluído no cran. De modo que você deverá fazer uso da versão em desenvolvimento. Há três razões para não incluir no cran:

1 - Pacotes para web scraping estão em constante desenvolvimento. Pǻginas web são alteradas ou mesmo inteiramente substituídas, o que pode obrigar o desenvolvedor a reescrever o pacote do zero;

2 - Os usuários do pacote serão em sua maioria brasileiros, e mesmo que não lusófonos venham a utilizá-lo, a familiaridade com a lingua portuguesa e com o direito brasileiro é indispensável;

3 -

Portanto, use sempre a versão em desenvolvimento: [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jjesusfilho/tjsp")
```

Código de conduta
-----------------

Antes de procedermos às instruções sobre como utilizar o pacote, algumas considerações que consideramos importantes:

1 - Este pacote foi criado por colaboradores voluntários. Você também é convidado a colaborar. Se encontrar uma falha nele, não hesite em criar um "issue", mas também procure fazer um esforço para corrigir você mesmo o erro e realizar um "pull request";

2 - O pacote é destinado principalmente ao público acadêmico, jornalistas e membros de organizações sem fins econômicos;

3 - Use o pacote com parcimônia. Não bombardeie a página do TJSP, pois a navegação de milhares de usuários poderá ser retardada. Você realmente precisa baixar tantas decisões? Precisa ser durante o dia, não pode ser à noite?

4 - Todas as funções de requisição possuem um argumento opcional chamado "usuario". Ele pode ser usado para você identificar-se ao provedor do serviço, fornecendo o seu contato. Essa tem sido uma prática considerada ética por web scrapers.

6 - Idealmente, o TJSP deveria disponibilizar uma Web API ou no mínimo um web service para facilitar o acesso a grandes volumes de decisões via comunicação máquina-máquina. Isso não custa tanto e não estaríamos criando esse raspador, como seguramente há muitos outros especialmente privados trabalhando nesse momento para fins comerciais. No entanto, o TJSP não proíbe expressamente o uso de raspadores, você pode conferir o que estou dizendo acessando o robots.txt do TJSP. Isso porém não quer dizer que seu IP não será bloqueado caso você decida reduzir o tempo entre requisições.

7 - Use os dados baixados para adicionar valor, por exemplo, para realizar análises ou publicar papers, fazer inferências, elaborar recomendações aos poderes públicos etc. Baixar esses dados para reproduzi-los em sua página web é tirar proveito do trabalho alheio, mesmo sendo esses dados públicos.

Como utilizar o pacote TJSP
---------------------------
