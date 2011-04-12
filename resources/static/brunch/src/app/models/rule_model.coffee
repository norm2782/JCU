class Rule extends Backbone.Model

  clear: ->
    @destroy()
    @view.remove()
