## what about the HTML function? have to add class and HTML tags, so in html.R
muRownamesPlain.character <- muPrintIdentity
muRownamesLatex.character <- muPrintIdentity

attr(muRownamesSummary, "plain.function") <- muRownamesPlain
attr(muRownamesSummary, "html.function") <-  muRownamesHTML
attr(muRownamesSummary, "latex.function") <-  muRownamesLatex

## what about the plain function? I think it uses muStratPlain.default,
## and only works because we give it one argument, 2 should insert a "/"
attr(muStratTest, "html.function") <- muStratTestHTML
attr(muStratTest, "latex.function") <- muStratTestLatex

attr(muResponseTest, "html.function") <- muStratTestHTML
attr(muResponseTest, "latex.function") <- muStratTestLatex
