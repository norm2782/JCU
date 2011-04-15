class RulesTreeItemView extends Backbone.View

  tagName:  "li"

  initialize: ->
    @model.view = @

  render: =>
    $(@el).html app.templates.rulesTreeItem content: @model.toJSON()
    @

  remove: ->
    $(@el).remove()

  clear: ->
    @model.clear()

