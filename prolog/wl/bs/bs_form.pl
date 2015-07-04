:- module(
  bs_form,
  [
    bs_input//3, % +Type:oneof([])
                 % +Id:atom
                 % +Label:atom
    bs_input_email//0,
    bs_input_password//0,
    bs_input_submit//0,
    bs_input_submit//1, % +Label:atom
    bs_input_text//2 % +Class:atom
                     % +Label:atom
  ]
).

/** <module> Bootstrap form support

Shortcuts for building Bootstrap-styles forms.

@author Wouter Beek
@version 2015/06-2015/07
*/

:- use_module(library(http/html_write)).





bs_input(Type, Class, Label) -->
  html(
    div(class='form-group', [
      label(for=Class, [Label,':']),
      input([class=['form-control',Class],name=Class,placeholder=Label,type=Type])
    ])
  ).

bs_input_email -->
  bs_input(email, email, 'Email').

bs_input_password -->
  bs_input(password, password, 'Password').

bs_input_submit -->
  bs_input_submit('Submit').

bs_input_submit(Label) -->
  html(button([class=[btn,'btn-default',submit],type=submit], Label)).

bs_input_text(Class, Label) -->
  bs_input(text, Class, Label).
