rulesTreeItemTemplate = require('templates/rules_tree_item')
RulesTreeNodeView = require('views/rules_tree_node_view').RulesTreeNodeView

class exports.RulesTreeNodeView extends Backbone.View

  tagName: "li"

  events:
    "click .btnDeleteTree"      : "deleteItem"
    "blur  .droppable"          : "checkRuleSyntax"
    "change input[type='text']" : "updateModel"

  initialize: ->
    _.bindAll(this, "render")
    @model.bind("change", @render)

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

  render: =>
    console.log "RulesTreeNodeView.render()"
    @$(@el).html rulesTreeItemTemplate content: @model.toJSON()
    @$(@el).droppable {
        hoverClass: 'dropHover'
      , drop: (event, ui) ->
          elem = $(this).find("input[type='text']")
          elem.val ui.draggable.find(".rule-text").html()
          elem.trigger('change')
      }

    @model.get('childRules').each @renderNode
    btn = $('<input type="button" value="New Field" />')
    btn.click @model, @newNode

    @$(@el).append btn
    @

  renderNode: (node) =>
    view = new RulesTreeNodeView model: node
    @$(@el).append view.render().el

  remove: ->
    @$(@el).remove()

  clear: ->
    @model.clear()

  updateModel: ->
    @model.set {rule: @$(@el).find("input[type='text']").val()}

  newNode: (e) ->
    console.log "RulesTreeNodeView.newNode()"
    model = e.data
    model.addRule()
