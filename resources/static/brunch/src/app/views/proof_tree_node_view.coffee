proofTreeItemTemplate = require('templates/proof_tree_item')
ProofTreeNodeView = require('views/proof_tree_node_view').ProofTreeNodeView

class exports.ProofTreeNodeView extends Backbone.View

  tagName: "li"

  events:
    "click .btnDeleteTree"      : "deleteItem"
    "blur  .droppable"          : "checkTermSyntax"
    "change input[type='text']" : "updateModel"

  initialize: =>
    @childTerms().bind "refresh", @render
    @model.bind "proof", @changeProofResult

  setBgColor: (fld, cls) =>
    fld.removeClass 'redField yellowField greenField whiteField'
    fld.addClass cls

  changeProofResult: =>
    switch @model.proofResult()
      when "Correct"    then bgc = 'greenField'
      when "Incomplete" then bgc = 'yellowField'
      when "Invalid"    then bgc = 'redField'
      else bgc = 'whiteField'

    @setBgColor @$(@el).find("input[type='text']"), bgc

  childTerms: =>
    @model.childTerms()

  checkTermSyntax: =>
    @updateModel()
    if !@model.isValid()
      bgc = "redField"
    else
      bgc = "whiteField"

    @setBgColor @$(@el).find("input[type='text']"), bgc

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
        view.model.setChildren(data)

    # TODO: Move this to a Model
    $.ajax
      url:  '/rules/unify'
      type: 'POST'
      contentType: 'application/json'
      dataType: 'json'
      data:     JSON.stringify {term: term, rule: rule}
      success:  callback
