:- module(
  bs_form,
  [
    bs_input//3, % +Type:oneof([])
                 % +Id:atom
                 % +Label:atom
    bs_input_email//1, % +Id:atom
    bs_input_email//2, % +Id:atom
                       % +Label:atom
    bs_input_password//1, % +Id:atom
    bs_input_password//2, % +Id:atom
                          % +Label:atom
    bs_input_submit//0,
    bs_input_submit//1, % +Label:atom
    bs_input_text//2 % +Id:atom
                     % +Label:atom
  ]
).

/** <module> Bootstrap form support

Shortcuts for building Bootstrap-styles forms.

@author Wouter Beek
@version 2015/06
*/

:- use_module(library(http/html_write)).





bs_input(Type, Id, Label) -->
  html(
    div(class=['form-group','has-feedback'], [
      label(for=Id, [Label,':']),
      input([class='form-control',id=Id,name=Id,placeholder=Label,type=Type])
    ])
  ).

bs_input_email(Id) -->
  bs_input_email(Id, 'Email').

bs_input_email(Id, Label) -->
  bs_input(email, Id, Label).

bs_input_password(Id) -->
  bs_input_password(Id, 'Password').

bs_input_password(Id, Label) -->
  bs_input(password, Id, Label).

bs_input_submit -->
  bs_input_submit('Submit').

bs_input_submit(Label) -->
  html(input([class=[btn,'btn-default'],type=submit,value=Label])).

bs_input_text(Id, Label) -->
  bs_input(text, Id, Label).
