class exports.Rule extends Backbone.Model
  validate: (str) ->
    if !str?
      str = @get "rule"

    token = "\\s*\\w+\\s*"
    rule  = token + "\\(" + token + "(," + token + ")*\\)\\s*"
    regex = new RegExp(rule + "(\\.|:-(" + rule + "(,\\s*|\\.))*)")
    regex.test str

  clear: ->
    @destroy()
    @view.remove()
