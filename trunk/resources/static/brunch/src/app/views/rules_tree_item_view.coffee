class RulesTreeItemView extends Backbone.View

  tagName:  "li"

  initialize: ->
    @model.view = @

  render: =>
    $(@el).html app.templates.rulesTreeItem content: @model.toJSON()
    $(@el).droppable {
        hoverClass: 'dropHover'
      , drop: (event, ui) ->
          # TODO: This is ugly... find a better way. Manually selecting
          # the first element and ditching all jQuery goodness should not
          # be necessary
          ruleSpan = ui.draggable.find(".rule-text")[0]
          $(this).find("input[type='text']").val ruleSpan.innerHTML }
    @

  remove: ->
    $(@el).remove()

  clear: ->
    @model.clear()

