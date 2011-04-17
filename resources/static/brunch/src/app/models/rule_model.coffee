class Rule extends Backbone.Model
  rule: ""

  clear: ->
    @destroy()
    @view.remove()
