let IMP_BUTTONS = ['run_analysis_btn', 'load-geo-btn']
// ====================================================================
// ------------- Diable all buttons
// ====================================================================
function disable_buttons()
{
	IMP_BUTTONS.forEach(btn_name => {
		document.getElementById(btn_name).disabled = true;
	});
}

function enable_buttons()
{
	IMP_BUTTONS.forEach(btn_name => {
		document.getElementById(btn_name).disabled = false;
	});
}

// ====================================================================
// ------------- Reset all
// ====================================================================
function RESET_ALL()
{
	// variable clear
	GSE_ID = "";
	PDATA = [];
	FDATA = [];
	DEG_RESULT = [];

	CASE_GROUPS = new Set();
	CONTROL_GROUPS = new Set();

	// table clear
	document.getElementById("pdata-viewer").innerHTML = "<thead></thead><tbody></tbody>";
	document.getElementById("fdata-viewer").innerHTML = "<thead></thead><tbody></tbody>";

	clear_dropdown_columns("pdata-dropdown-container");
	clear_dropdown_columns("fdata-dropdown-container");

	// clear deg table
	document.querySelector("#deg-viewer thead").innerHTML = "";
	document.querySelector("#deg-viewer tbody").innerHTML = "";

	// clear all plots
	const image_id = [
		'boxplot-edata-img', 'densityplot-edata-img', 'pca-edata-img', 'hclust-edata-img', 'volcano-edata-img',
		'gene-boxplot-img'
	]
	image_id.forEach(id_name => {
		document.getElementById(id_name).src = "";
	})

	// reset analysis options
	reset_analysis_options();
	

}


// ====================================================================
// ------------- load patient data into table and sort column
// ====================================================================

function load_data_into_table(table_id, data, sort_callback, callback2){

	const my_table = document.getElementById(table_id);
	my_table.innerHTML = "<thead></thead><tbody></tbody>";

	const header_name = Object.keys(data[0]);

	// console.log(header_name)

    
	let header_row = document.createElement("tr");
	for(let name of header_name){
		let th = document.createElement('th');
		th.innerText = name;

		th.addEventListener( 'click',  () => sort_column(table_id, name, data, sort_callback) );

		header_row.appendChild(th);
	}
	
	my_table.querySelector('thead').appendChild(header_row);


	let tbody = my_table.querySelector('tbody')
	for( let row of data){
		let tr = document.createElement('tr');
		for(key in row){
			let td = document.createElement('td');
			td.innerText = row[key];
			tr.appendChild(td);

            if (typeof callback2 === 'function') {
                callback2(key, tr, row);
            }
			// // load previous history
			// if(key == 'PID'){
			// 	if(CASE_GROUPS.has(row[key])){
			// 		tr.classList.add('case');
			// 	} else if (CONTROL_GROUPS.has(row[key])){
			// 		tr.classList.add('control');
			// 	}
			// }
		}
		tr.addEventListener('click', ()=>{
			tr.classList.toggle("selected");
		});

		tbody.appendChild(tr);
	}

}


function sort_column(table_id, name, obj, callback){
	let asc;

	let my_table = document.getElementById(table_id);

	// console.log(name, my_table);

	my_table.querySelectorAll("thead tr th").forEach((node)=>{
		if(node.innerText == name){
			asc = (node.classList.contains('asc')) ? true : false;
			return;
		}
	});


	// sort list of objects
	obj.sort((a, b) => {
		if (asc){
			return (a[name] > b[name]) ? -1 : 1;
		  	
		} else {
			return (a[name] < b[name]) ? -1 : 1;	
		}
	});
	

	load_data_into_table(table_id, obj, callback);
    if (typeof callback === 'function') {
        callback();
    }

	my_table.querySelectorAll("thead tr th").forEach((node)=>{
		if(node.innerText == name){
			// console.log(node);
			node.classList.remove();				
			(asc) ? node.classList.add('des') : node.classList.add('asc');
			return;
		}
	});
}



// ------------------------ select patients ---------------------------
function clear_all(){
	document.querySelectorAll("#pdata-viewer tbody tr").forEach(row =>{
		row.classList.value = "";
	});

	CONTROL_GROUPS.clear();
	CASE_GROUPS.clear();
}

function unselect(all){
	document.querySelectorAll("#pdata-viewer tbody tr.selected").forEach(row =>{
		row.classList.remove('selected');

		if(all){
			let pid = row.querySelector('td').innerText;
			if(row.classList.contains('control')){
				row.classList.remove('control');
				CONTROL_GROUPS.delete(pid);
				console.log(CONTROL_GROUP);

			} else if(row.classList.contains('case')){
				row.classList.remove('case');
				CASE_GROUPS.delete(pid);
				console.log(CASE_GROUPS);
			}
		}

	})

}

function select_pid(control=true){
	let selected_rows = document.querySelectorAll("#pdata-viewer tbody tr.selected");

	for(let node of selected_rows){

		if(control){
			CONTROL_GROUPS.add(node.querySelector('td').innerText);
		} else {
			CASE_GROUPS.add(node.querySelector('td').innerText);
		}

		node.classList.value = (control) ? "control" : "case";
	}

	(control) ? console.log(CONTROL_GROUPS) : console.log(CASE_GROUPS)

}


// ====================================================================
// ------------- Dropdown select columns
// ====================================================================
function create_column_dropdown(container_id, dropdown_id, table_id) {
  const wrapper = document.getElementById(container_id);

  wrapper.innerHTML = `
    <button onclick="toggle_column_dropdown('${dropdown_id}')" class="w3-button w3-white w3-border">
      Columns &#9660;
    </button>
    <div id="${dropdown_id}" class="w3-dropdown-content w3-bar-block w3-light-grey w3-border w3-card-4 r-dropdown-box">
      <div class="w3-bar-item w3-center">
        <button class="w3-button w3-round-small w3-small w3-ripple w3-blue"
                onclick="select_all_columns('${dropdown_id}', true)">Select All</button>
        <button class="w3-button w3-round-small w3-small w3-ripple w3-blue"
                onclick="select_all_columns('${dropdown_id}', false)">Unselect All</button>
        <button class="w3-button w3-round-small w3-small w3-ripple w3-blue"
                onclick="set_columns('${dropdown_id}', '${table_id}')">Set</button>
      </div>
    </div>
  `;
}

function toggle_column_dropdown(dropdown_id){
    const dropdown_content= document.getElementById(dropdown_id);
    if(dropdown_content.style.display == 'block') dropdown_content.style.display = 'none';
    else dropdown_content.style.display = 'block';
}


function initilize_dropdown_columns(dropdown_id, table_id, is_checked){
    const dropdown_content= document.getElementById(dropdown_id);
    const thead = document.querySelectorAll(`#${table_id} thead th`);

	clear_dropdown_columns(dropdown_id);

    thead.forEach((th, index)=>{
        // colnames.push(column);
        // <label class="w3-bar-item"><input type="checkbox" class="w3-check" value="col1"> Column 1</label>
        const label = document.createElement('label');
        label.className = "w3-bar-item";

        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.className = 'w3-check';
        checkbox.value = index;
        checkbox.checked = is_checked;

        label.appendChild(checkbox);
        label.appendChild(document.createTextNode(th.innerText));

        dropdown_content.appendChild(label);
    });
}

function clear_dropdown_columns(dropdown_id){
    const dropdown_content= document.getElementById(dropdown_id);
    const labels = dropdown_content.querySelectorAll("label");
    labels.forEach(label => label.remove());
}

function select_all_columns(dropdown_id, is_selected){
    const dropdown_content= document.getElementById(dropdown_id);
    const checkbox = dropdown_content.querySelectorAll("label input");
    checkbox.forEach(cb => cb.checked = is_selected)
}

function set_columns(dropdown_id, table_id){
    const dropdown_content= document.getElementById(dropdown_id);
    const checkbox = dropdown_content.querySelectorAll("label input");

    let seleted_columns = [];
    checkbox.forEach(cb => { if(cb.checked) seleted_columns.push( parseInt(cb.value) ) } );

    const thead = document.querySelector(`#${table_id} thead`);
    const tbody = document.querySelector(`#${table_id} tbody`);

    // Filter thead
    Array.from(thead.querySelectorAll("th")).forEach((th, i) => {
      th.style.display = seleted_columns.includes(i) ? "" : "none";
    });

    // Filter tbody
    Array.from(tbody.rows).forEach(row => {
      Array.from(row.cells).forEach((cell, i) => {
        cell.style.display = seleted_columns.includes(i) ? "" : "none";
      });
    });

}



// ====================================================================
// ------------- update groupby
// ====================================================================
function update_groupby_from_pdata(select_id){
    const select_obj = document.getElementById(select_id);
    select_obj.innerHTML = `<option value="condition">condition</option>`;

    Object.keys(PDATA[0]).forEach(colnames => {
		if(colnames != "PID")
		{
			const option_el = document.createElement('option');
			option_el.value = colnames;
			option_el.innerText = colnames;
			select_obj.appendChild(option_el);
		}
    });
}

