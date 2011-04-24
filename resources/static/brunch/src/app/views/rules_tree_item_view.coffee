rulesTreeItemTemplate = require('templates/rules_tree_item')

class exports.RulesTreeItemView extends Backbone.View

  tagName: "li"

  events:
    "click .btnDeleteTree" : "deleteItem"
    "blur  .droppable"     : "checkRuleSyntax"
    "change input[type='text']" : "updateModel"


  checkRuleSyntax: ->
    fld = @$(@el).find("input[type='text']")

    if !@model.validate fld.val()
      bgc = "#faa"
    else
      bgc = "#fff"

    fld.css "background-color", bgc

  deleteItem: ->
    @model.destroy()
    @$(@el).remove()

  initialize: ->
    @model.view = @

  render: =>
    @$(@el).html rulesTreeItemTemplate content: @model.toJSON(), isTerm: @options.isTerm
    @$(@el).droppable {
        hoverClass: 'dropHover'
      , drop: (event, ui) ->
          # TODO: Update collection after drop
          newVal = ui.draggable.find(".rule-text").html()
          $(this).find("input[type='text']").val newVal
      }
    @

  remove: ->
    @$(@el).remove()

  clear: ->
    @model.clear()

  updateModel: ->
    @model.set {rule: @$(@el).find("input[type='text']").val()}
