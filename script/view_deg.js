
let dfe_results;


// -------------------------- define functions ---------------------------
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

			tr.addEventListener('click', ()=>{
				tr.classList.toggle("selected");
			});

			tbody.appendChild(tr);
		}
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


// --------------- highlight genes -----------------------
function clear_file(){
	let gene_file = document.getElementById('mark-gene-file');
	gene_file.value = '';
}

function load_gene_names(){

	return new Promise((resolve, reject) => {
		let genes = new Set();
		let input_box_genes = new Set(document.getElementById('gene-names').value.split(/[\t\n\s,]+/));

		const reader = new FileReader();
		reader.onload = (event) => {
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

			reader.readAsText(gene_file.files[0]);
		}
	});	
}


function show_input_genes() {
	load_gene_names()
	.then(gene_names =>{
		console.log(gene_names);

		if ( gene_names.size == 0 ) {
			alert('No Input Genes!')
		} else{
			let new_results = [];

			dfe_results.forEach(obj =>{
				if(gene_names.has(obj.Gene)){
					new_results.push(obj);
				}
			});

			console.log(new_results);
			load_data_into_table('result-table', new_results);
		}
	});
}


// -------------------------- body ---------------------------

dfe_results = JSON.parse(localStorage.getItem('dfe_results'));

console.log(dfe_results);

load_data_into_table('result-table', dfe_results);


















