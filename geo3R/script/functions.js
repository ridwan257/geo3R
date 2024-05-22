let query_genes, metadata, gseID, expression_data, feature_data, dfe_results;
let default_volcano_img, tmp, IP;
let case_group = new Set();
let control_group = new Set();


// ----------------------------- functions ---------------------------------
// --------- delete local storage while closing -----------------
window.addEventListener('beforeunload', (event) => {

    // localStorage.removeItem('dfe_results');


    event.returnValue = '';
});

// ------------ Display loading screen -----------------------
function showLoadingScreen(msg='Loading...') {
  document.getElementById('loadingScreen').style.display = 'flex';
  document.querySelector('#loadingScreen p').innerHTML = msg;
}

// ---------------- Hide loading screen ----------------------
function hideLoadingScreen() {
  document.getElementById('loadingScreen').style.display = 'none';
}


// ------------- load patient data into table and sort column -----------
function load_data_into_table(table_id, data){

	let my_table = document.getElementById(table_id);
	my_table.innerHTML = "<thead></thead><tbody></tbody>";

	let header_name = Object.keys(data[0]);

	// console.log(header_name)

	let header_row = document.createElement("tr");
	for(let name of header_name){
		let th = document.createElement('th');
		th.innerText = name;

		th.addEventListener( 'click', () => sort_column(table_id, name, data) );

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

			// load previous history
			if(key == 'PID'){
				if(case_group.has(row[key])){
					tr.classList.add('case');
				} else if (control_group.has(row[key])){
					tr.classList.add('control');
				}
			}
		}
		tr.addEventListener('click', ()=>{
			tr.classList.toggle("selected");
		});




		tbody.appendChild(tr);
	}

}



function sort_column(table_id, name, obj){
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
	


	load_data_into_table(table_id, obj);

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

	control_group.clear();
	case_group.clear();
}

function unselect(all){
	document.querySelectorAll("#pdata-viewer tbody tr.selected").forEach(row =>{
		row.classList.remove('selected');

		if(all){
			let pid = row.querySelector('td').innerText;
			if(row.classList.contains('control')){
				row.classList.remove('control');
				control_group.delete(pid);
				console.log(control_group);

			} else if(row.classList.contains('case')){
				row.classList.remove('case');
				case_group.delete(pid);
				console.log(case_group);
			}
		}

	})

}

function select_pid(control=true){
	let selected_rows = document.querySelectorAll("#pdata-viewer tbody tr.selected");

	for(let node of selected_rows){

		if(control){
			control_group.add(node.querySelector('td').innerText);
		} else {
			case_group.add(node.querySelector('td').innerText);
		}

		node.classList.value = (control) ? "control" : "case";
	}

	(control) ? console.log(control_group) : console.log(case_group)

}

// ------------------ clear file volcano ---------------------
function clear_file_volcano(){
	let gene_file = document.getElementById('mark-gene-file');
	gene_file.value = '';
}



// ---------------------- load gene names -----------------------
function load_gene_names(){

	return new Promise((resolve, reject) => {
		let genes = new Set();
		let input_box_genes = new Set(document.getElementById('gene-names').value.split(/[\t\n\s,]+/));

		const reader1 = new FileReader();
		reader1.onload = (event) => {
			let contents = new Set(event.target.result.split(/[\t\n\s,]+/));
			contents.forEach(g => genes.add(g));

			input_box_genes.forEach(g => genes.add(g));

			genes.delete('');

			resolve(genes);

		}

		let gene_file = document.getElementById('mark-gene-file');
		if(gene_file.files.length == 0 ){
			input_box_genes.forEach(g => genes.add(g));

			genes.delete('');

			resolve(genes);

		} else{
			reader1.readAsText(gene_file.files[0]);
		}
	});	
}


// --------------- get the differentially expressed genes ---------------
function get_deg_gene(){
	let gene_names = [];

	let alpha = Number(document.getElementById('alpha-value').value);
	let test_stat_name = document.getElementById('p-value').value;
	let logFC = [];
	Array.from(document.getElementsByClassName('logFC')).forEach( el => logFC.push(Number(el.value)) );
	console.log(logFC);
	
	let state = '';
		
	if( dfe_results ){
		dfe_results.forEach( row => {
			if ( (row.logFC <= logFC[0] || row.logFC >= logFC[1]) && row[test_stat_name] <= alpha ){
				
				switch(true){
					case row.logFC <= logFC[0]:
						state = -1;
						break;
					case row.logFC >= logFC[1]:
						state = 1;
						break;
					default:
						state = 0;
				}

				gene_names.push({
					'name' : row.Gene,
					'state' : state
				});
			}
		});
	}

	return gene_names;
}



// ---------------------- handling pop up box ------------------------
function load_to_clipboard(x){
	return new Promise( (res, rej)=>{
		navigator.clipboard.writeText(x)
		.then(() => {
		  console.log('Text copied to clipboard!');
		  res();
		})
		.catch( err => rej(err) );
	});
	
}

function show_pop_up_box(){
	let gene_list_container = document.querySelector('#gene-list div');
	gene_list_container.innerHTML = '';
	let color = ['red', 'black', 'blue'];

	get_deg_gene().forEach(gene => {
		let p = document.createElement('p');
		p.innerText = gene.name;
		p.style.color = color[gene.state+1];
		gene_list_container.appendChild(p);
	});

	document.getElementById('gene-list').style = 'display:block;';
}


function hide_pop_up_box(){
	document.getElementById('gene-list').style = 'display:none;';
}



// ------------------- create object from tsv file ------------------------
function tsv_dump(file_handler){
	
	return new Promise( (resolve, reject) => {
		let reader = new FileReader();
		let obj = [];

		reader.onload = event => {
			let contents = event.target.result.split('\n');
			let headers = contents[0].split('\t').map( td => td.trim() );
			
			for (let i = 1; i < contents.length; i++){
				let tr = contents[i].split('\t').map( td => td.trim() );
				
				if ( tr.length == headers.length){
					let row = {};
					for(let j=0; j < headers.length; j++){
						row[headers[j]] = tr[j];
					}
					obj.push(row);
				}	
			}

			resolve(obj);
		}

		reader.onerror = (event) => {
		  reject(event.target.error);
		};


		reader.readAsText(file_handler);

	});
}











