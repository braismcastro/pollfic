-module(gui).
-include("../include/wx.hrl").
-export([start/0, init/0]).
%erl -smp -noshell -s gui start -s init stop

init() ->
	spawn(?MODULE, start, []).

start() -> 
	register(master, self()),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	%	%	%	%	%	INICIALIZACIÓN DEL FRAME	%	%	%	%	%	
	Wx=wx:new(),
	F=wxFrame:new(Wx, ?wxID_ANY, "PollFic", [{size, {760, 400}}]),
	wxFrame:show(F),

	wxFrame:createStatusBar(F),
	wxFrame:setStatusText(F, "...."), 
	MenuBar = wxMenuBar:new(), 
	wxFrame:setMenuBar (F, MenuBar),
	OptMn = wxMenu:new(), 
	wxMenuBar:append (MenuBar, OptMn, "&Options"), 

	Quit = wxMenuItem:new ([{id,400},{text, "&Quit"}]), 
	wxMenu:append (OptMn, Quit),

	HelpMn = wxMenu:new(),
	wxMenuBar:append (MenuBar, HelpMn, "&Help"), 
	About = wxMenuItem:new ([{id,500},{text,"About"}]),
	wxMenu:append (HelpMn, About),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	%	%	%	INICIALIZACIÓN DE LAS DOS SECCIONES 	%	%	%	

	Panel = wxPanel:new(F, []),
	List = config_list(Panel),   
    
	%%SECCION DE VOTO
		RadioBox = wxRadioBox:new(Panel, 1, "",
			      ?wxDefaultPosition,
			      ?wxDefaultSize,
			      ["Sí", "No"],
			      [{majorDim, 3},
			       {style, ?wxHORIZONTAL}]),
   		wxRadioBox:connect(RadioBox, command_radiobox_selected),

		VoteButton = wxButton:new(Panel, ?wxID_ANY, [{label, "Votar!"}]),	
		VoteSection = wxBoxSizer:new(?wxVERTICAL),
		wxSizer:add(VoteSection, RadioBox, [{proportion, 0}, {flag, ?wxEXPAND}]),
		wxSizer:add(VoteSection, VoteButton, [{proportion, 0} ]),
	%%%%%%

	%%SECCION INFO_POLL
		PollName = wxStaticText:new(Panel, 1, "", []),
		PollDescription = wxTextCtrl:new(Panel, ?wxID_ANY, [{style, ?wxTE_WORDWRAP 
			bor ?wxTE_MULTILINE bor ?wxTE_READONLY}, {size, {370,200}}]),
		PollSection = wxBoxSizer:new(?wxVERTICAL),
		wxSizer:add(PollSection, PollName, [{proportion, 0}]),
	    wxSizer:add(PollSection, PollDescription, [{proportion, 0}, {flag, ?wxEXPAND}]),
		wxSizer:add(PollSection, VoteSection, [{proportion, 0}, {flag, ?wxEXPAND}]),

	%%%%%%%
	ListDivider = wxBoxSizer:new(?wxVERTICAL),
	RefreshButton = wxButton:new(Panel, ?wxID_ANY, [{label, "Refresh"}]),	
	wxSizer:add(ListDivider, List, [{proportion, 0}, {flag, ?wxEXPAND}] ),
	wxSizer:add(ListDivider, RefreshButton, [{proportion, 0}, {flag, ?wxEXPAND}]),
	MainDivider = wxGridSizer:new(1, 2, 2, 2),
    wxSizer:add(MainDivider, ListDivider, [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainDivider, PollSection, [{proportion, 0}, {flag, ?wxEXPAND}]),
    
    wxPanel:setSizer(Panel, MainDivider),
    wxSizer:fit(MainDivider, Panel),

    %%%%%%%%%%%%HANDLERS%%%%%%%%%%%%%%%%%%%
    %AL VOTAR A UNA ENCUESTA
    VoteHandler = fun(_,_) ->
    				wxFrame:setStatusText(F, "Votando..."), 
					Index = wxRadioBox:getSelection(RadioBox),
					Name = wxStaticText:getLabel(PollName),
					ListIndex = wxListCtrl:findItem(List, 0, Name),
					{Name, Ip, Port} = get_tuple(List, ListIndex),
					client:vote({Ip, Port, Name}, checkvote(Index)),
					wxFrame:setStatusText(F, "Votado!")
				  end,
	wxButton:connect(VoteButton, command_button_clicked, [{callback,VoteHandler }]),

	%AL CLICKAR EN UN ELEMENTO DE LA LISTA
	PollDetailsHandler = fun(#wx{obj = _ListCtrl, event = #wxList{itemIndex = Item}},_) ->
		    	wxFrame:setStatusText(F, "Pidiendo información de la encuesta..."), 

		    	{Name, Ip, Port} = get_tuple(List, Item),
				{{poll_inf,{Name,Descr}},{positive,N},{negative,M}} = client:poll_details({Ip, Port, Name}),
				
				DetailedDescr = io_lib:format("Descripción: \n ~p \n\n\n Votos positivos: ~p \n \n Votos negativos: ~p", [Descr, N, M]),				
				wxStaticText:setLabel(PollName, Name),
    			wxTextCtrl:setValue(PollDescription, DetailedDescr),
    			show(VoteButton, PollName, PollDescription, RadioBox),
    			wxFrame:setStatusText(F, "Encuesta conseguida: " ++ Name)			 
					end,
	wxListCtrl:connect(List, command_list_item_selected, [{callback,PollDetailsHandler }]),

	%%AL PEDIR POLLS A DISCOVER
    RefreshHandler = fun(_,_) ->
    		wxFrame:setStatusText(F, "Pidiendo encuestas al discover..."), 
			L = client:find_polls(),
			wxListCtrl:deleteAllItems(List),
			insertList(L, List),
			hide(VoteButton, PollName, PollDescription, RadioBox),
			wxFrame:setStatusText(F, "Lista actualizada!")
		    end,
	wxButton:connect(RefreshButton, command_button_clicked, [{callback,RefreshHandler }]),

	CloseHandler = fun (_,_) ->
				master ! {stop, F}
			end,
	wxFrame:connect (F, close_window, [{callback, CloseHandler}] ),
	

	hide(VoteButton, PollName, PollDescription, RadioBox),
	loop().

loop() ->
	receive
		{stop, F} -> unregister(master),
					wxFrame:destroy(F)
	end.

checkvote(0) -> positive;
checkvote(1) -> negative.

%%Para insertar la lista del discover en la lista gráfica
insertList([], _) -> ok;
insertList([{Ip, Port, PollName}|T], List) ->
	insert(List, PollName, Ip, Port),
	insertList(T, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%FUNCIONES AUXILIARES PARA SIMPLIFICAR LA API%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hide(VoteButton, PollName, PollDescription, RadioBox) -> 
	wxButton:hide(VoteButton),
	wxStaticText:hide(PollName),
	wxTextCtrl:hide(PollDescription),
	wxRadioBox:hide(RadioBox).

show(VoteButton, PollName, PollDescription, RadioBox) -> 
	wxButton:show(VoteButton),
	wxStaticText:show(PollName),
	wxTextCtrl:show(PollDescription),
	wxRadioBox:show(RadioBox).


get_tuple(List, Index) -> 
	Item = wxListItem:new(),
	wxListItem:setId(Item, Index),
	wxListItem:setColumn(Item, 0),
	wxListItem:setMask(Item, ?wxLIST_MASK_TEXT),
	wxListCtrl:getItem(List, Item),
	Col1 = wxListItem:getText(Item),

	Item2 = wxListItem:new(),
	wxListItem:setId(Item2, Index),
	wxListItem:setColumn(Item2, 1),
	wxListItem:setMask(Item2, ?wxLIST_MASK_TEXT),
	wxListCtrl:getItem(List, Item2),
	Col2 = wxListItem:getText(Item2),

	Item3 = wxListItem:new(),
	wxListItem:setId(Item3, Index),
	wxListItem:setColumn(Item3, 2),
	wxListItem:setMask(Item3, ?wxLIST_MASK_TEXT),
	wxListCtrl:getItem(List, Item3),
	Col3 = wxListItem:getText(Item3),

	{Col1,
	 util:str_to_term(Col2), 
	 util:str_to_term(Col3)}.

insert(List, PollName, Ip, Port) ->
    wxListCtrl:insertItem(List, 0, 0),
    wxListCtrl:setItem(List, 0,0, PollName),
    wxListCtrl:setItem(List, 0,1,util:term_to_str(Ip)),
    wxListCtrl:setItem(List, 0,2,util:term_to_str(Port)).

config_list(Panel) -> 
 	List= wxListCtrl:new(Panel, [{style, ?wxLB_SINGLE}, {size, {380,350}}]),
    wxListCtrl:insertColumn(List, 0, "Nombre"),
    wxListCtrl:insertColumn(List, 1, "IP"),
    wxListCtrl:insertColumn(List, 2, "Puerto"),
    wxListCtrl:setColumnWidth(List, 0, 200),
    wxListCtrl:setColumnWidth(List, 1, 120),
    wxListCtrl:setColumnWidth(List, 2, 60),
    List.
