proofTreeItemTemplate = require('templates/proof_tree_item')
ProofTreeNodeView = require('views/proof_tree_node_view').ProofTreeNodeView

# TODO: Refactor setting background color. Instead of setting color, set CSS class
class exports.ProofTreeNodeView extends Backbone.View

  tagName: "li"

  events:
    "click .btnDeleteTree"      : "deleteItem"
    "blur  .droppable"          : "checkTermSyntax"
    "change input[type='text']" : "updateModel"

  initialize: =>
    @childTerms().bind "refresh", @render
    @model.bind "proof", @changeProofResult

  changeProofResult: =>
    switch @model.proofResult()
      when "Correct"    then bgc = '#66ff66'
      when "Incomplete" then bgc = '#ffff66'
      when "Invalid"    then bgc = '#ff6666'
      else bgc = '#ffffff'

    @$(@el).find("input[type='text']").css "background-color", bgc

  childTerms: =>
    @model.childTerms()

  checkTermSyntax: =>
    @updateModel()
    if !@model.isValid()
      bgc = "#faa"
    else
      bgc = "#fff"

    @$(@el).find("input[type='text']").css "background-color", bgc

  deleteItem: =>
    @model.destroy()
    @$(@el).remove()

  render: =>
    view = @
    @$(@el).html proofTreeItemTemplate content: @model.toJSON()
    @$(@el).find(".dropzone").droppable {
        hoverClass: 'dropHover'
      , drop: (event, ui) ->
          elemVal = $(this).find("input[type='text']").val()
          if !elemVal
            alert "There needs to be a term in the text field!"
            @
          else
            view.model.setTerm elemVal

            if !view.model.isValid()
              alert "Cannot unify with an invalid term!"
              @
            else
              view.unify(elemVal, ui.draggable.find(".rule-text").html())
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
    @model.setTerm @$(@el).find("input[type='text']").val()

  unify: (term, rule) =>
    view = @
    callback = (data) ->
      if !data.unified
        alert "Failed to unify!"
      else
        view.model.setChildNo(data.children)

    # TODO: Move this to a Model
    $.ajax
      url:  '/rules/unify'
      type: 'POST'
      contentType: 'application/json'
      dataType: 'json'
      data:     JSON.stringify {term: term, rule: rule}
      success:  callback
