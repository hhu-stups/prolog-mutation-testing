:- module(compute_sicstus_coverage,[coverage_score/4]).

/*
 * SICStus measures coverage of program lines. If one line of a predicate is not covered, we assume the predicate to be not tested.
 * There are different ways of interpreting the coverage results of the SICStus prolog coverage analysis.
 * `Type = subgoal` 
 * `Type = clause` computes the coverage on the level of clauses, that is, we view a predicate's clause to be uncovered if any coverage site within this clause is indicated to be uncovered.
 * `Type = predicate` computes the coverage on the level of predicates which we view as uncovered if they contain an uncovered clause.
 *
 * Usage:
 * - Filename is the file under test
 * - start SICStus interpreter session
 * - `prolog_flag(source_info,_,on).`
 * - `[Filename]`
 * - `prolog_flag(profiling,_,on).`
 * - `run_tests.`
 * - `prolog_flag(profiling,_,off).`
 * - `[compute_sicstus_coverage].`
 * - `coverage_data(D) , coverage_score(Type,Filename,D,Score).`
 *
 * M = bc_interpreter , prolog_flag(source_info,_,on) , [M] , prolog_flag(profiling,_,on) , M:run_tests , prolog_flag(profiling,_,off) , use_module('../compute_sicstus_coverage') , coverage_data(D) , coverage_score(subgoal,M,D,Score).
 *
 */

coverage_score(subgoal,Filename,CoverageData,CoverageScore) :- 
    atom_concat(Filename,'.pl',FilenameExt) , 
    findall(CC, (member(counter(FilePath,_,_,_)-C, CoverageData) , atom_concat(_,FilenameExt,FilePath) , get_single_coverage(C,CC)), L) ,
    length(L, LL) , 
    findall(0, member(0,L) , ZL) , 
    length(ZL, ZLL) ,
    CoverageScore is 1 - ZLL / LL.

coverage_score(clause,Filename,CoverageData,CoverageScore) :- 
    atom_concat(Filename,'.pl',FilenameExt) , 
    filter_and_join_preds_of_interest(clause,FilenameExt,CoverageData,[],FilteredPreds) , 
    findall(Uncovered,(member(Uncovered,FilteredPreds) , Uncovered = counter(_,_,_,_)-0),UncoveredPreds) , 
    length(FilteredPreds,LFP) , 
    length(UncoveredPreds,LUP) , 
    (LFP == 0 -> CoverageScore = 0 ; CoverageScore is 1 - (LUP / LFP)).

coverage_score(predicate,Filename,CoverageData,CoverageScore) :- 
    atom_concat(Filename,'.pl',FilenameExt) , 
    filter_and_join_preds_of_interest(predicate,FilenameExt,CoverageData,[],FilteredPreds) , 
    findall(Uncovered,(member(Uncovered,FilteredPreds) , Uncovered = counter(_,_,_,_)-0),UncoveredPreds) , 
    length(FilteredPreds,LFP) , 
    length(UncoveredPreds,LUP) , 
    (LFP == 0 -> CoverageScore = 0 ; CoverageScore is 1 - (LUP / LFP)).

filter_and_join_preds_of_interest(_,_,[],Acc,Acc).
filter_and_join_preds_of_interest(CoverageType,Filename,[counter(FilePath,PredName,Index,Line)-Coverage|T],Acc,FilteredPreds) :- 
    atom_concat(_,Filename,FilePath) , ! , 
    accumulate_pred_coverage(CoverageType,counter(FilePath,PredName,Index,Line)-Coverage,Acc,NewAcc) , 
    filter_and_join_preds_of_interest(CoverageType,Filename,T,NewAcc,FilteredPreds).
filter_and_join_preds_of_interest(CoverageType,Filename,[_|T],Acc,FilteredPreds) :- 
    filter_and_join_preds_of_interest(CoverageType,Filename,T,Acc,FilteredPreds).

accumulate_pred_coverage(CoverageType,counter(FilePath,PredName,Index,Line)-Coverage,[Previous|RAcc],NewAcc) :- 
    Previous = counter(_,PrevPredName,PrevIndex,_)-PrevCoverage , 
    (CoverageType == clause -> Index == PrevIndex ; true) , 
    PredName == PrevPredName , ! , 
    get_single_coverage(Coverage,CCoverage) , 
    KeepCoverage is min(CCoverage,PrevCoverage) , 
    NewAcc = [counter(FilePath,PredName,Index,Line)-KeepCoverage|RAcc].
accumulate_pred_coverage(_CoverageType,counter(FilePath,PredName,Index,Line)-Coverage,Acc,[counter(FilePath,PredName,Index,Line)-CCoverage|Acc]) :- 
    get_single_coverage(Coverage,CCoverage).

get_single_coverage(nondet(C),C).
get_single_coverage(det(C),C).
get_single_coverage(C,C) :- number(C).
