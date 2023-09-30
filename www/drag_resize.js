/*
// get x y coordinate relative to top-left
const aa = document.getElementById("rec2")
aa.getAttribute("data-x")
aa.getAttribute("data-y")
// get box size
aa.offsetHeight
aa.offsetWidth
*/

// https://shiny.posit.co/r/articles/build/communicating-with-js/
/*
Shiny.setInputValue("foo", "bar", {priority: "event"});
This will cause input$foo to notify any reactive objects that depend on it, whether its value has actually changed or not.
*/

const sizeMap = new Map(); // position and size information
const contentMap = new Map(); // size information

// get position information for a id
// all positions and size are relative to the width and height, respectively
function getPos(id){
  let aa = document.getElementById("drawing-area");
  let bb = document.getElementById(id);
  let x = bb.getAttribute('data-x')/aa.offsetWidth;
  let y = bb.getAttribute('data-y')/aa.offsetHeight;
  let width = bb.offsetWidth/aa.offsetWidth;
  let height = bb.offsetHeight/aa.offsetHeight;
  return {x: x, y:y, width: width, height: height};
}

// document.body.addEventListener('dblclick', function (evt) {
//     if (evt.target.className.indexOf('resize-drag') > -1 ) {
//         evt.target.remove()
//         // remove keys
//         sizeMap.delete(evt.target.id);
//         contentMap.delete(evt.target.id);
//     }
// }, false);

$(document).on('shiny:inputchanged', function(event) {
	const boxA = document.getElementById("drawing-area");
  let convertNum = 96*2; // for inch, 96px is 1 inch
  if (document.getElementById("unit").value == "mm") convertNum = 3.78*2; // 1mm=3.78px
  let boxAwidth = document.getElementById("label_width").value * convertNum;
  if (boxAwidth > boxA.parentElement.clientWidth * 0.9) boxAwidth = boxA.parentElement.clientWidth * 0.9;
	// boxA.style.height = document.getElementById("label_height").value * convertNum + "px";
	// boxA.style.width = document.getElementById("label_width").value * convertNum + "px";
  boxA.style.width = boxAwidth + "px";
	boxA.style.height = document.getElementById("label_height").value / document.getElementById("label_width").value * boxAwidth + "px";
	/*document.getElementById("label_height").addEventListener("input", e => {
	  boxA.style.height = e.target.value*2 + "in";
	});
	document.getElementById("label_width").addEventListener("input", e => {
	  boxA.style.width = e.target.value*2 + "in";
	});*/
});

// add new box in the label
let idnum = 1;
// add a rect box in the drawing area
function addBox(){
  let container = document.getElementById("drawing-area");
  let input_type = document.getElementById("input_type").value;
  let id = 'rec' + idnum;
  idnum += 1;
  if (input_type === 'text') {
    const box = document.createElement("div");
    box.innerHTML = document.getElementById("input_var").value;
    console.log(document.getElementById("input_var").value);
    box.id = id;
    box.classList.add("resize-drag");
    box.style.fontSize = 8*96/72 + "px"// document.getElementById("input_var_size").value *96/72 + "px";//96px = 72pt= 1 inch
    box.style.color = document.getElementById("input_var_color").value;
    box.style.fontFamily = document.getElementById("input_var_fontfamily").value;
    let fontface = document.getElementById("input_var_fontface").value;
    if (fontface == 2) box.style.fontWeight = 'bold';
    else if (fontface == 3) box.style.fontStyle = 'italic';
    else if (fontface == 4) {box.style.fontWeight = 'bold'; box.style.fontStyle = 'italic';}
    container.appendChild(box);
  } else {
    const box = document.createElement("img");
    box.id = id;
    box.src = document.getElementById("barcodeImg").src;
    box.classList.add("resize-drag");
    if (input_type != 'linear') box.classList.add("square"); // add a new class for preserving ratio when resizing
    container.appendChild(box);
  }
  sizeMap.set(id, getPos(id));
  var e = document.getElementById("input_var");
  var text = e.options[e.selectedIndex].text;
  let fontcolor = document.getElementById("input_var_color").value;
  let fontfamily = document.getElementById("input_var_fontfamily").value;
  if (fontfamily == "sans-serif") fontfamily = "serif";
  else if (fontfamily == "monospace") fontfamily = "mono";
  let fontface = document.getElementById("input_var_fontface").value;
  contentMap.set(id, {type: input_type, var: text, fontcolor: fontcolor, fontfamily: fontfamily, fontface: fontface})
  Shiny.setInputValue("sizeInfo", Object.fromEntries(sizeMap), {priority: "event"});
  Shiny.setInputValue("contentInfo", Object.fromEntries(contentMap), {priority: "event"});
}
/*
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'addButton') {
	  var container = document.getElementById("drawing-area");
	  const box = document.createElement("div");
	  box.innerHTML = document.getElementById("input_var").value;
	  console.log(document.getElementById("input_var").value);
	  box.id = 'rec' + idnum;
	  idnum += 1;
	  box.classList.add("resize-drag");
	  container.appendChild(box);
  }
});
*/

// send size information to R
function sendSize (event) {
  var target = event.target
  sizeMap.set(target.id, getPos(target.id));//update the position information before sending
  Shiny.setInputValue("sizeInfo", Object.fromEntries(sizeMap), {priority: "event"});
  Shiny.setInputValue("contentInfo", Object.fromEntries(contentMap), {priority: "event"});
}

// resize and drag
const myPreserveRatio = interact.modifiers.aspectRatio({ ratio: 'preserve', enabled: false })
const interactable = interact('.resize-drag')
interactable
  .resizable({
    // resize from all edges and corners
    edges: { left: false, right: true, bottom: true, top: false },

    listeners: {
      move (event) {
        var target = event.target
        var x = (parseFloat(target.getAttribute('data-x')) || 0)
        var y = (parseFloat(target.getAttribute('data-y')) || 0)

        // if new object has class "square" (for 2D barcodes), preserveRatio when resizing
        if ( target.classList.contains("square") ) {
          myPreserveRatio.enable()
        } else {
          myPreserveRatio.disable()
        }

        // update the element's style
        target.style.width = event.rect.width + 'px'
        target.style.height = event.rect.height + 'px'

        // translate when resizing from top or left edges
        x += event.deltaRect.left
        y += event.deltaRect.top

        target.style.transform = 'translate(' + x + 'px,' + y + 'px)'

        target.setAttribute('data-x', x)
        target.setAttribute('data-y', y)
        /*target.textContent = Math.round(event.rect.width) + '\u00D7' + Math.round(event.rect.height)*/
      }
    },
    modifiers: [
      // keep the edges inside the parent
      interact.modifiers.restrictEdges({
        outer: 'parent'
      }),

      // minimum size
      interact.modifiers.restrictSize({
        min: { width: 20, height: 20 }
      }),
      myPreserveRatio
    ],

    inertia: true
  })

  .draggable({
    listeners: { move: window.dragMoveListener },
    inertia: true,
    modifiers: [
      interact.modifiers.restrictRect({
        restriction: 'parent',
        endOnly: true
      })
    ]
  })
  // .on('dragend', showEventInfo)
  .on(['dragend', 'resizeend'], sendSize)
  .on('doubletap', function (evt) {
    evt.target.remove()
    // remove keys
    sizeMap.delete(evt.target.id);
    contentMap.delete(evt.target.id);
    Shiny.setInputValue("sizeInfo", Object.fromEntries(sizeMap), {priority: "event"});
    Shiny.setInputValue("contentInfo", Object.fromEntries(contentMap), {priority: "event"});
})


// drag and move listener
function dragMoveListener (event) {
  var target = event.target
  // keep the dragged position in the data-x/data-y attributes
  var x = (parseFloat(target.getAttribute('data-x')) || 0) + event.dx
  var y = (parseFloat(target.getAttribute('data-y')) || 0) + event.dy

  // translate the element
  target.style.transform = 'translate(' + x + 'px, ' + y + 'px)'

  // update the posiion attributes
  target.setAttribute('data-x', x)
  target.setAttribute('data-y', y)
}

// this function is used later in the resizing and gesture demos
window.dragMoveListener = dragMoveListener
