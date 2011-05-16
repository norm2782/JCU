class exports.Term extends Backbone.Model
  validate: (str) ->
    if !str?
      str = @get "term"

    # TODO: Modify this Rule regex to become a Term regex
    # Token -> a word with possibly spaces in front and after
    # Rule  -> Token ( Token {, Token}* )
    # Regex -> Rule {, Rule}* {. | :- { Rule {, | .} }* }
    token = "\\s*\\w+\\s*"
    rule  = token + "\\(" + token + "(," + token + ")*\\)\\s*"
    regex = new RegExp(rule + "(\\.|:-(" + rule + "(,\\s*|\\.))*)")
    # regex = new RegExp(rule + "(," + rule + ")*" + "(\\.|:-(" + rule + "(,\\s*|\\.))*)") -- TODO: Enable after modifying server-side parser
    regex.test str

  clear: ->
    @destroy()
    @view.remove()
