class exports.Rule extends Backbone.Model
  # Attributes:
  # rule :: String

  validate: (str) =>
    if !str?
      if !@get('rule')?
        return false
      str = @get "rule"

    # Token -> a word with possibly spaces in front and after
    # Rule  -> Token ( Token {, Token}* )
    # Regex -> Rule {, Rule}* {. | :- { Rule {, | .} }* }
    token = "\\s*\\w+\\s*"
    rule  = token + "\\(" + token + "(," + token + ")*\\)\\s*"
    regex = new RegExp("\\s*^" + rule + "(:-(" + rule + ",\\s*)*\\s*(" + rule +
                       "\\s*))?\\s*\\.?\\s*$")
    console.log regex
    regex.test str
