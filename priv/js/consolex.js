var commandHistory = [];
if (commandHistoryStored = Lockr.get('commandHistory')) {
    commandHistory = commandHistoryStored;
}

var editor = CodeMirror.fromTextArea(document.getElementById("editor"), {
    lineNumbers: true,
    mode: "elixir",
    theme: "text-ex-machina",
    extraKeys: {
        'Ctrl-Enter': function(){executeCode()}
    }
});

var consoleLog = CodeMirror.fromTextArea(document.getElementById("console-log"), {
    theme: "text-ex-machina",
    readOnly: true
});

$(".task-modal").modal("show");
$("input[name=task]").change(function(){
    task = this.value
    if(task == "other") {
        $("#other-task-input").show()
    } else {
        $("#other-task-input").hide()
    }
})

$(".launch-shell").click(function(){
    task_choice = $("input[name=task]:checked").val()
    if(task_choice == "other") {
        task = $("#other-task-input").val()
    } else {
        task = task_choice
    }
    startShell(task);
})

ws = null;
if (!window.WebSocket) {
  alert("WebSocket not supported by this browser");
}

function go() {
    ws = new WebSocket("ws://" + location.host + "/websocket");
    ws.onopen = function () {}
    ws.onclose = function () {
      consoleLog.setValue("Launch again\nReason : disconnected from shell, shell terminated")
      go()
    }
    ws.onmessage = function (e) {        
        if(!isTerminated) {
            updateConsoleLog(e.data, false)
        }
    }
}

function startShell(task) {
    msg = JSON.stringify({task: task})
    ws.send(msg);
    isTerminated = false
}

function executeCode() {
    msg = editor.getValue()
    updateConsoleLog(msg, true)
    // commandHistory = [msg]
    commandHistory.push(msg)
    console.log(commandHistory)
    Lockr.set('commandHistory', commandHistory);
    ws.send(msg)
}

var isTerminated = false;
$(".terminate-shell-btn").click(function(){
    msg = JSON.stringify({task: "terminate"})
    ws.send(msg);
    isTerminated = true
    ws = null
    go()
})


function updateConsoleLog(data, isInput){
    var doc = consoleLog.getDoc();
    var cursor = doc.getCursor();
    var initialSize = consoleLog.doc.size
    var line = doc.getLine(cursor.line);
    
    var pos = {
        line: (doc.size+5),
        ch: line.length - 1
    }
    doc.replaceRange('\n'+data, pos);
    var finalSize = consoleLog.doc.size
    if(isInput) {
        for(var i=initialSize; i<=finalSize; i++) {
            console.log(i)
            consoleLog.addLineClass(i, 'wrap', 'console-log-input')
        }
    }
}

go();

/*$('sendForm').onsubmit = function (event) {
      var p = $('phrase');
      ws.send(p.value);
      p.value='';
      return false;
}*/