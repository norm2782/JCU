rulesTreeItemTemplate = require('templates/rules_tree_item')

class exports.RulesTreeItemView extends Backbone.View

  tagName: "li"

  events:
    "click .btnDeleteTree" : "deleteItem"

  deleteItem: ->
    @model.destroy()
    @$(@el).remove()

  initialize: ->
    @model.rule = 'foo'
    @model.view = @

  render: =>
    @$(@el).html rulesTreeItemTemplate content: @model.toJSON(), isTerm: @options.isTerm
    @$(@el).droppable {
        hoverClass: 'dropHover'
      , drop: (event, ui) ->
          # TODO: This is ugly... find a better way. Manually selecting
          # the first element and ditching all jQuery goodness should not
          # be necessary
          ruleSpan = ui.draggable.find(".rule-text")[0]
          $(this).find("input[type='text']").val ruleSpan.innerHTML
      }
    @

  remove: ->
    @$(@el).remove()

  clear: ->
    @model.clear()

