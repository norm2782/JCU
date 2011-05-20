proofTreeItemTemplate = require('templates/proof_tree_item')
ProofTreeNodeView = require('views/proof_tree_node_view').ProofTreeNodeView
Term = require('models/term_model').Term

class exports.ProofTreeNodeView extends Backbone.View

  tagName: "li"

  events:
    "click .btnDeleteTree"      : "deleteItem"
    "blur  .droppable"          : "checkTermSyntax"
    "change input[type='text']" : "updateModel"

  initialize: =>
    @childTerms().bind "refresh", @render

  childTerms: =>
    @model.childTerms()

  checkTermSyntax: =>
    fld = @$(@el).find("input[type='text']")
    tm = new Term()
    if !tm.validate fld.val()
      bgc = "#faa"
    else
      bgc = "#fff"

    fld.css "background-color", bgc

  deleteItem: =>
    @model.destroy()
    @$(@el).remove()

  render: =>
    view = @
    @$(@el).html proofTreeItemTemplate content: @model.toJSON()
    @$(@el).find(".dropzone").droppable {
        hoverClass: 'dropHover'
      , drop: (event, ui) ->
          elem = $(this).find("input[type='text']")
          if !elem.val()
            alert "There needs to be a term in the text field!"
            @
          else
            term = new Term()
            elemVal = elem.val()

            if !term.validate(elemVal)
              alert "Cannot unify with an invalid term!"
              @
            else
              view.unify(elem, elemVal, ui.draggable.find(".rule-text").html())
      }

    if @childTerms().length > 0
      ul = $('<ul></ul>')
      renderNode = (node) ->
        nodeView = new ProofTreeNodeView({model: node})
        ul.append nodeView.render().el

      @childTerms().each renderNode
      @$(@el).append ul
    @

  updateModel: =>
    @model.set {term: new Term({term: @$(@el).find("input[type='text']").val()})}

  unify: (elem, term, rule) =>
    view = @
    callback = (data) ->
      if !data.unified
        alert "Failed to unify!"
      else
        view.model.setChildNo(data.children)
        # elem.trigger('change') # DOM change, not Backbone change

    # TODO: Move this to a Model
    $.ajax
      url:  '/rules/unify'
      type: 'POST'
      contentType: 'application/json'
      dataType: 'json'
      data:     JSON.stringify {term: term, rule: rule}
      success:  callback
