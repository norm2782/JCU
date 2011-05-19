proofTreeItemTemplate = require('templates/proof_tree_item')
ProofTreeNodeView = require('views/proof_tree_node_view').ProofTreeNodeView
Rule = require('models/rule_model').Rule

class exports.ProofTreeNodeView extends Backbone.View

  tagName: "li"
  tmpUl: null

  events:
    "click .btnDeleteTree"      : "deleteItem"
    "blur  .droppable"          : "checkTermSyntax"
    "change input[type='text']" : "updateModel"

  initialize: =>
    @model.bind "change", @render

  childTerms: =>
    @model.get('childTerms')

  checkTermSyntax: =>
    fld = @$(@el).find("input[type='text']")
    rl = new Rule()
    if !rl.validate fld.val()
      bgc = "#faa"
    else
      bgc = "#fff"

    fld.css "background-color", bgc

  deleteItem: =>
    @model.destroy()
    @$(@el).remove()

  render: =>
    console.log "render"

    model = @
    @$(@el).append proofTreeItemTemplate content: @model.toJSON()
    @$(@el).find(".dropzone").droppable {
        hoverClass: 'dropHover'
      , drop: (event, ui) ->
          elem = $(this).find("input[type='text']")
          if !elem.val()
            alert "You need to have entered a term in the textfield!"
            @
          else
            rule = new Rule()
            elemVal = elem.val()

            if !rule.validate(elemVal)
              alert "Cannot unify with an invalid term!"
              @
            else
              model.unify(elem, elemVal, ui.draggable.find(".rule-text").html())
      }

    @tmpUl = $('<ul></ul>')
    @childTerms().each @renderNode
    @$(@el).append @tmpUl
    @tmpUl = null
    @

  renderNode: (node) =>
    view = new ProofTreeNodeView model: node
    @tmpUl.append view.render().el

  updateModel: =>
    @model.set {rule: @$(@el).find("input[type='text']").val()}

  unify: (elem, term, rule) =>
    view = @
    callback = (data) ->
      if !data.unified
        alert "Failed to unify!"
      else
        view.model.setChildNo(data.children)
        elem.trigger('change') # DOM change, not Backbone change

    # TODO: Move this to a Model
    $.ajax
      url:  '/rules/unify'
      type: 'POST'
      contentType: 'application/json'
      dataType: 'json'
      data:     JSON.stringify {term: term, rule: rule}
      success:  callback
