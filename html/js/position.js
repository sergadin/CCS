var board;

var describeField = function(square, piece) {
    $.get("/position/?square=" + square + "&piece=" + piece, function(data) {
        $('#data').html( data );
    });
}

var init_board = function() {
    var onSquareTimeoutID;


    var removeGreySquares = function() {
        $('#board .square-55d63').css('background', '');
    };

    var greySquare = function(square) {
        var squareEl = $('#board .square-' + square);

        var background = '#a9a9a9';
        if (squareEl.hasClass('black-3c85d') === true) {
            background = '#696969';
        }

        squareEl.css('background', background);
    };

    var onMouseoverSquare = function(square, piece) {
        if( !piece )
            return;

        // highlight the square they moused over
        greySquare(square);

        onSquareTimeoutID = window.setTimeout(function() {
            describeField(square, piece);
        }, 1000);
    };

    var onMouseoutSquare = function(square, piece) {
        removeGreySquares();
        window.clearTimeout( onSquareTimeoutID );
    };


    var onDragStart = function(source, piece, position, orientation) {
        // do not pick up pieces if the game is over
        // or if it's not that side's turn
        if ((orientation === 'white' && piece.search(/^w/) === -1) ||
            (orientation === 'black' && piece.search(/^b/) === -1)) {
            return false;
        }
    };

    var onDrop = function(source, target) {
        removeGreySquares();

        // do not alow to make a move
        return 'snapback';
    };

    var cfg = {
        showNotation: false,
        position: 'start',
        draggable: true,
        onDragStart: onDragStart,
        onDrop: onDrop,
        onMouseoutSquare: onMouseoutSquare,
        onMouseoverSquare: onMouseoverSquare
    };
    board = ChessBoard('board', cfg);
}; // end init()
