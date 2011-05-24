proofTreeItemTemplate = require('templates/proof_tree_item')
ProofTreeNodeView = require('views/proof_tree_node_view').ProofTreeNodeView

class exports.ProofTreeNodeView extends Backbone.View

  tagName: "li"

  events:
    "blur .droppable" : "onBlurCheckTermSyntax"

  txtFld: =>
    $("input[id='proof_" + @model.get('treeLbl') + "']")

  initialize: =>
    @childTerms().bind "refresh", @render
    @model.bind "proof", @changeProofResult

  setBgColor: (fld, cls) =>
    fld.removeClass 'redField yellowField greenField whiteField blueField'
    fld.addClass cls

  changeProofResult: =>
    switch @model.proofResult()
      when "Correct"    then bgc = 'greenField'
      when "Incomplete" then bgc = 'yellowField'
      when "Invalid"    then bgc = 'redField'
      else bgc = 'whiteField'

    @setBgColor @txtFld(), bgc

  childTerms: =>
    @model.childTerms()

  onBlurCheckTermSyntax: =>
    @model.setTerm @txtFld().val()

    view = @
    callback = (data) ->
       # TODO: Error message
      # console.log data
      # data[0] : Boolean indicating whether we have a successful parse or not
      # data[1] : List of error strings indicating what went wroning during parsing
      if data[0]
        bgc = "whiteField"
        valid = true
      else
        bgc = "blueField"
        valid = false
      view.setBgColor view.txtFld(), bgc
      view.model.setValidSyntax valid

    $.ajax
      type:  'POST'
      url:   "/check-syntax/term"
      data:  @model.term()
      contentType: 'application/json'
      dataType: 'json'
      success:  callback

  render: =>
    view = @
    @$(@el).html proofTreeItemTemplate content: @model.toJSON()
    @$(@el).find(".dropzone").droppable {
        hoverClass: 'dropHover'
      , drop: (event, ui) ->
          elemVal = $(this).find("input[type='text']:first").val()
          if !elemVal
            alert "There needs to be a term in the text field!"
            @
          else
            view.model.setTerm elemVal

            if !view.model.hasValidSyntax()
              alert "Cannot unify with an invalid term!"
              @
            else
              view.unify(elemVal, ui.draggable.find(".rule-text").html())
      }

    if @childTerms().length > 0
      ul = $('<ul></ul>')
      renderNode = (node) ->
        nodeView = new ProofTreeNodeView({ model: node
                                         , id: "view_" + node.treeLbl})
        ul.append nodeView.render().el

      @childTerms().each renderNode
      @$(@el).append ul
    @

  unify: (term, rule) =>
    view = @
    callback = (data) ->
      if !data.unified
        alert "Failed to unify!"
      else
        view.model.setChildren data

    # TODO: Move this to a Model
    $.ajax
      url:  '/rules/unify'
      type: 'POST'
      contentType: 'application/json'
      dataType: 'json'
      data:     JSON.stringify {term: term, rule: rule, tree: JSON.stringify app.models.tree.treeRoot()}
      success:  callback
