// SIMILARITY JUDEGEMENT EXPERIMENT 
// Overview: 
//      (1) Helper
//      (2) Parameters and Stimulus Setup 
//      (3) Control Flow

//TODO: add RT

// ---------------- 1. HELPER ------------------
// show slide function
function showSlide(id) {
    $(".slide").hide(); //jquery - all elements with class of slide - hide
    $("#" + id).show(); //jquery - element with given id - show
}

//array shuffle function
shuffle = function(o) { //v1.0
    for (var j, x, i = o.length; i; j = parseInt(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
}

// this gets the data from googgle sheet (specidied below)
function init() {
    Tabletop.init( { key: public_spreadsheet_url,
                     callback: getTrials,
                     simpleSheet: true } );
}

function getTrials(data) {
    // data comes through as a simple array since simpleSheet is turned on
    //alert("Successfully processed " + data.length + " rows!")
}

// ---------------- 2. EXPERIMENT SETUP ------------------

// Define experimental parameters
var paper_width = 674;
var paper_height = 100;
var circle_width = 20
var ISI = 1000
var public_spreadsheet_url = 'https://docs.google.com/spreadsheets/d/1gwF2wVZrZqrcnTDYp165zEYoSNipZCufD2mUsHeg4ms/edit?usp=sharing';
var rating_q = "How <b> similar </b> in appearance are these two drawings?"
var num_trials = 10 

// Initialize experiment logic variables
var centerX = paper_width / 2;
var centerY = paper_height / 2;
var paper = Raphael("drawing_canvas", paper_width, paper_height);
var num_ratings = -1 //initialize at -1
var current_likert_value
var current_rt

// Get and load images
var objs = []
for (i = 1; i < (num_trials/2) + 1; i++) {
    objs.push(i + "_A_Fin.jpg")
    objs.push(i + "_B_Fin.jpg")   
}

objs = shuffle(objs)

// preload  images
$.fn.preload = function() {
  this.each(function(){
        $('<img/>')[0].src = this;
    });
};


// ---------------- 3. CONTROL FLOW ------------------
window.addEventListener('DOMContentLoaded', init) // get data from google spreadsheet

// START experiment
showSlide("consent");

// MAIN EXPERIMENT
var experiment = {
    
    // SHOW INSTRUCTION SLIDE
    instructions: function () {
        start_condition = (window.self == window.top | turk.workerId.length > 0) 
        if (start_condition) {
            showSlide("instructions")
        }
    },

    // SELECT NEXT ACTION
    next: function() {

        num_ratings++ // increment trial
        paper.clear();

        if (num_ratings == 0) {
           experiment.get_ratings()
        } else {
            experiment.save()
        }
    },

    // GET RATINGS DISPLAY FUNCTION
    get_ratings: function() {
        
        rating_image1_html = '<img src=images/' + objs[num_ratings] + ' alt="' + objs[num_ratings] + '" id="objImage" class = "objImage"/>';
        $("#ratingimage1").html(rating_image1_html)

        rating_image2_html = '<img src=images/' + objs[num_ratings + 1] + ' alt="' + objs[num_ratings + 1] + '" id="objImage" class = "objImage"/>';
        $("#ratingimage2").html(rating_image2_html)

        rating_question_html = '<p style="font-size:24px;text-align: justify;">' + rating_q + '</p>';
        $("#ratingquestion").html(rating_question_html)

        var circle1 = paper.circle(centerX - 150,centerY,circle_width).attr({
                        fill: "white", 
                        stroke: "rgb(0,0,0)",
                        "stroke-width": 5,
                      })
     
        var circle2 = paper.circle(centerX - 100, centerY, circle_width).attr({
                        fill: "white", 
                        stroke: "rgb(0,0,0)",
                        "stroke-width": 5,
                      })

        var circle3 = paper.circle(centerX - 50,centerY,circle_width).attr({
                        fill: "white", 
                        stroke: "rgb(0,0,0)",
                        "stroke-width": 5,
                      })
  
        var circle4 = paper.circle(centerX,centerY,circle_width).attr({
                        fill: "white", 
                        stroke: "rgb(0,0,0)",
                        "stroke-width": 5,
                      })

        var circle5 = paper.circle(centerX + 50,centerY,circle_width).attr({
                        fill: "white", 
                        stroke: "rgb(0,0,0)",
                        "stroke-width": 5,
                      })

        var circle6 = paper.circle(centerX + 100,centerY,circle_width).attr({
                        fill: "white", 
                        stroke: "rgb(0,0,0)",
                        "stroke-width": 5,
                      })


        var circle7 = paper.circle(centerX + 150,centerY,circle_width).attr({
                        fill: "white", 
                        stroke: "rgb(0,0,0)",
                        "stroke-width": 5,
                      })

        setTimeout(function(){ // this is a hack:  https://github.com/DmitryBaranovskiy/raphael/issues/491
            paper.text(centerX - 150, centerY, "1").attr({"font-size": 18});
            paper.text(centerX - 100, centerY, "2").attr({"font-size": 18});
            paper.text(centerX - 50, centerY, "3").attr({"font-size": 18,});
            paper.text(centerX, centerY, "4").attr({"font-size": 18,});
            paper.text(centerX + 50 , centerY, "5").attr({"font-size": 18,});
            paper.text(centerX + 100 , centerY, "6").attr({"font-size": 18,});
            paper.text(centerX + 150 , centerY, "7").attr({"font-size": 18,});

            paper.text(91, centerY, "Almost Identical").attr({"font-size": 16, "font-weight":"bold"});
            paper.text(596, centerY, "Completely Different").attr({"font-size": 16, "font-weight":"bold"});
        });
        $("#counter").html(num_ratings + 1 + ' / ' + objs.length)


        $(document).keyup(function(event){
              var keys = {"1": 49, "2": 50, "3": 51, "4": 52, "5": 53, "6": 54, "7": 55};
              switch(event.which) {
                case keys["1"]:
                   circle1.attr("fill", "red");
                   $(document).unbind("keyup")
                   endTime = (new Date()).getTime()
                   current_rt = endTime - startTime
                   setTimeout(experiment.next, ISI); 
                   current_likert_value = 1 
                   break;

                case keys["2"]:
                   circle2.attr("fill", "red");
                   $(document).unbind("keyup")
                   endTime = (new Date()).getTime()
                   current_rt = endTime - startTime
                   setTimeout(experiment.next, ISI); 
                   current_likert_value = 2
                   break;

                case keys["3"]:
                   circle3.attr("fill", "red");
                   $(document).unbind("keyup")
                   endTime = (new Date()).getTime()
                   current_rt = endTime - startTime
                   setTimeout(experiment.next, ISI); 
                   current_likert_value = 3
                   break;

                case keys["4"]:
                   circle4.attr("fill", "red");
                   $(document).unbind("keyup")
                   endTime = (new Date()).getTime()
                   current_rt = endTime - startTime
                   setTimeout(experiment.next, ISI);
                   current_likert_value = 4 
                   break;

                case keys["5"]:
                   circle5.attr("fill", "red");
                   $(document).unbind("keyup")
                   endTime = (new Date()).getTime()
                   current_rt = endTime - startTime
                   setTimeout(experiment.next, ISI); 
                   current_likert_value = 5
                   break;

                case keys["6"]:
                   circle6.attr("fill", "red");
                   $(document).unbind("keyup")
                   endTime = (new Date()).getTime()
                   current_rt = endTime - startTime
                   setTimeout(experiment.next, ISI); 
                   current_likert_value = 6
                   break;

                case keys["7"]:
                   circle7.attr("fill", "red");
                   $(document).unbind("keyup")
                   endTime = (new Date()).getTime()
                   current_rt = endTime - startTime
                   setTimeout(experiment.next, ISI); 
                   current_likert_value = 7
                   break;
                }
          })

       // start timer
       var startTime = (new Date()).getTime()
       showSlide("complex_rating")    

    }, 


    // SAVE DATA
    save: function() {

        eval('experiment.rating_' + num_ratings + ' =' + current_likert_value)
        eval('experiment.drawing_id_' + num_ratings + ' = "' + objs[num_ratings - 1] + '"')
        eval('experiment.RT_' + num_ratings + ' = "' + current_rt + '"')


        // go to next trial
        if (num_ratings == objs.length) {
            experiment.end()
        } else {
            experiment.get_ratings()
        }
    },

    // END FUNCTION 
    end: function() {

        showSlide("finished");
        setTimeout(function() {

            turk.submit(experiment)}, 500)
    }
}